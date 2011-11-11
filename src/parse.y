//
//  Copyright (C) 2011  Nick Gasson
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

%code top {
   #include "util.h"
   #include "ident.h"

   #include <sys/types.h>
   #include <sys/mman.h>
   #include <sys/stat.h>
   #include <fcntl.h>
   #include <unistd.h>
   #include <string.h>
   #include <stdarg.h>
}

%code requires {
   #include "tree.h"

   #include <stdbool.h>

   #define YYLTYPE loc
   typedef struct loc loc;

   #define YYLLOC_DEFAULT(Current, Rhs, N) {                         \
         if (N) {                                                    \
            (Current).first_line   = YYRHSLOC(Rhs, 1).first_line;    \
            (Current).first_column = YYRHSLOC(Rhs, 1).first_column;  \
            (Current).last_line    = YYRHSLOC(Rhs, N).last_line;     \
            (Current).last_column  = YYRHSLOC(Rhs, N).last_column;   \
            (Current).file         = YYRHSLOC(Rhs, N).file;          \
            (Current).linebuf      = YYRHSLOC(Rhs, 1).linebuf;       \
         }                                                           \
         else {                                                      \
            (Current).first_line   = (Current).last_line   =         \
               YYRHSLOC(Rhs, 0).last_line;                           \
            (Current).first_column = (Current).last_column =         \
               YYRHSLOC(Rhs, 0).last_column;                         \
            (Current).file = YYRHSLOC(Rhs, 0).file;                  \
            (Current).linebuf = YYRHSLOC(Rhs, 0).linebuf;            \
         }                                                           \
      }

   typedef struct {
      int64_t ival;
      char    *sval;
      char    cval;
   } lvals_t;

   typedef struct id_list {
      struct id_list *next;
      ident_t        id;
   } id_list_t;

   typedef struct unit_list {
      struct unit_list *next;
      unit_t           unit;
   } unit_list_t;

   typedef struct assoc_list {
      struct assoc_list *next;
      assoc_t           assoc;
   } assoc_list_t;

   typedef struct param_list {
      struct param_list *next;
      param_t           param;
   } param_list_t;

   typedef struct tree_list {
      struct tree_list *next;
      tree_t           value;
   } tree_list_t;
}

%code provides {
   bool input_from_file(const char *file);

   tree_t parse(void);
   void begin_token(char *tok);
   int get_next_char(char *b, int max_buffer);
   int parse_errors(void);
}

%code {
   extern lvals_t lvals;
   extern YYLTYPE yylloc;

   static int        n_errors = 0;
   static const char *read_ptr;
   static const char *file_start;
   static size_t     file_sz;
   static const char *perm_linebuf = NULL;
   static const char *perm_file_name = NULL;
   static int        n_token_next_start = 0;
   static int        n_row = 0;
   static bool       last_was_newline = true;
   static tree_t     root;

   int yylex(void);
   static void yyerror(const char *s);

   static void copy_trees(tree_list_t *from,
                          void (*copy_fn)(tree_t t, tree_t d),
                          tree_t to);
   static void copy_params(param_list_t *from,
                           void (*copy_fn)(tree_t t, param_t p),
                           tree_t to);
   static id_list_t *id_list_add(id_list_t *list, ident_t id);
   static id_list_t *id_list_append(id_list_t *a, id_list_t *b);
   static void id_list_free(id_list_t *list);
   static assoc_list_t *assoc_list_add(assoc_list_t *list, assoc_t a);
   static void assoc_list_free(assoc_list_t *list);
   static param_list_t *param_list_add(param_list_t *list, param_t a);
   static void param_list_free(param_list_t *list);
   static unit_list_t *unit_list_add(unit_list_t *list, unit_t id);
   static void unit_list_free(unit_list_t *list);
   static void tree_list_append(tree_list_t **l, tree_t t);
   static void tree_list_prepend(tree_list_t **l, tree_t t);
   static void tree_list_concat(tree_list_t **a, tree_list_t *b);
   static void tree_list_free(tree_list_t *l);
   static tree_t build_expr1(const char *fn, tree_t left,
                             const struct YYLTYPE *loc);
   static tree_t build_expr2(const char *fn, tree_t left, tree_t right,
                             const struct YYLTYPE *loc);
   static tree_t str_to_agg(const char *start, const char *end);
   static void parse_error(const loc_t *loc, const char *fmt, ...);
}

%union {
   tree_t       t;
   ident_t      i;
   tree_list_t  *l;
   struct {
      tree_list_t *left;
      tree_list_t *right;
   } p;
   id_list_t    *s;
   assoc_list_t *a;
   param_list_t *c;
   assoc_t      b;
   port_mode_t  m;
   type_t       y;
   range_t      r;
   unit_t       u;
   unit_list_t  *v;
}

%type <t> entity_decl opt_static_expr expr abstract_literal literal
%type <t> numeric_literal library_unit arch_body process_stmt conc_stmt
%type <t> seq_stmt timeout_clause physical_literal target severity
%type <t> package_decl name aggregate string_literal report
%type <t> waveform waveform_element seq_stmt_without_label
%type <t> comp_instance_stmt conc_stmt_without_label elsif_list
%type <i> id opt_id selected_id func_name
%type <l> interface_signal_decl interface_object_decl interface_list
%type <l> port_clause generic_clause interface_decl signal_decl
%type <l> block_decl_item arch_decl_part arch_stmt_part process_decl_part
%type <l> variable_decl process_decl_item seq_stmt_list type_decl
%type <l> subtype_decl package_decl_part package_decl_item enum_lit_list
%type <l> constant_decl formal_param_list subprogram_decl name_list
%type <l> sensitivity_clause process_sensitivity_clause
%type <p> entity_header
%type <s> id_list context_item context_clause selected_id_list use_clause
%type <m> opt_mode
%type <y> subtype_indication type_mark type_def scalar_type_def
%type <y> integer_type_def physical_type_def enum_type_def array_type_def
%type <y> index_subtype_def
%type <y> unconstrained_array_def constrained_array_def
%type <r> range range_constraint index_constraint constraint
%type <u> base_unit_decl
%type <v> secondary_unit_decls
%type <a> element_assoc_list
%type <b> element_assoc
%type <c> param_list generic_map port_map

%token tID tENTITY tIS tEND tGENERIC tPORT tCONSTANT tCOMPONENT
%token tCONFIGURATION tARCHITECTURE tOF tBEGIN tFOR tTYPE tTO
%token tALL tIN tOUT tBUFFER tBUS tUNAFFECTED tSIGNAL tDOWNTO
%token tPROCESS tWAIT tREPORT tLPAREN tRPAREN tSEMI tASSIGN tCOLON
%token tCOMMA tINT tSTRING tERROR tINOUT tLINKAGE tVARIABLE tIF
%token tRANGE tSUBTYPE tUNITS tPACKAGE tLIBRARY tUSE tDOT tNULL
%token tTICK tFUNCTION tIMPURE tRETURN tPURE tARRAY tBOX tASSOC
%token tOTHERS tASSERT tSEVERITY tON tMAP tTHEN tELSE tELSIF

%left tAND tOR tNAND tNOR tXOR tXNOR
%left tEQ tNEQ tLT tLE tGT tGE
%left tSLL tSRL tSLA tSRA tROL tROR
%left tPLUS tMINUS tAMP
%left tTIMES tOVER tMOD tREM
%left tPOWER
%nonassoc tABS tNOT

%error-verbose
%expect 0

%%

design_unit
: context_clause library_unit
  {
     for (id_list_t *it = $1; it != NULL; it = it->next)
        tree_add_context($2, it->id);
     id_list_free($1);

     root = $2;
     YYACCEPT;
  }
| /* empty */ {}
;

context_clause
: context_item context_clause
  {
     $$ = id_list_append($2, $1);
  }
| /* empty */ { $$ = NULL; }
;

context_item : library_clause { $$ = NULL; } | use_clause ;

library_clause
: tLIBRARY id_list tSEMI
  {
     // TODO: foreach(id_list) { load_library(..); }
     id_list_free($2);
  }
;

use_clause
: tUSE selected_id_list tSEMI
  {
     $$ = $2;
  }
;

library_unit : entity_decl | arch_body | package_decl ;

id
: tID
  {
     $$ = ident_new(lvals.sval);
     free(lvals.sval);
  }
;


selected_id
: id tDOT selected_id
  {
     $$ = ident_prefix($1, $3, '.');
  }
| id
| tALL { $$ = ident_new("all"); }
;

id_list
: id
  {
     $$ = id_list_add(NULL, $1);
  }
| id tCOMMA id_list
  {
     $$ = id_list_add($3, $1);
  }
;

selected_id_list
: selected_id
  {
     $$ = id_list_add(NULL, $1);
  }
| selected_id tCOMMA selected_id_list
  {
     $$ = id_list_add($3, $1);
  }
;

opt_id : id { $$ = $1; } | { $$ = NULL; } ;

entity_decl
: tENTITY id tIS entity_header /* entity_decl_part */ tEND
  opt_entity_token opt_id tSEMI
  {
     $$ = tree_new(T_ENTITY);
     tree_set_ident($$, $2);
     tree_set_loc($$, &@$);
     copy_trees($4.left, tree_add_generic, $$);
     copy_trees($4.right, tree_add_port, $$);

     if ($7 != NULL && $7 != $2) {
        parse_error(&@7, "%s does not match entity name %s",
                    istr($7), istr($2));
     }
  }
;

opt_entity_token : tENTITY | /* empty */ ;

entity_header
: generic_clause port_clause
  {
     $$.left  = $1;
     $$.right = $2;
  }
;

generic_clause
: tGENERIC tLPAREN interface_list tRPAREN tSEMI { $$ = $3; }
| /* empty */ { $$ = NULL; }
;

package_decl
: tPACKAGE id tIS package_decl_part tEND opt_package_token opt_id tSEMI
  {
     $$ = tree_new(T_PACKAGE);
     tree_set_ident($$, $2);
     copy_trees($4, tree_add_decl, $$);

     if ($7 != NULL && $7 != $2) {
        parse_error(&@7, "%s does not match package name %s",
                    istr($7), istr($2));
     }
  }
;

opt_package_token : tPACKAGE | /* empty */ ;

package_decl_part
: package_decl_item package_decl_part
  {
     $$ = $1;
     tree_list_concat(&$$, $2);
  }
| /* empty */
  {
     $$ = NULL;
  }
;

package_decl_item
: type_decl
| subtype_decl
| constant_decl
| subprogram_decl
/* | signal_declaration
   | shared_variable_declaration
   | file_declaration
   | alias_declaration
   | component_declaration
   | attribute_declaration
   | attribute_specification
   | disconnection_specification
   | use_clause
   | group_template_declaration
   | group_declaration */
;

port_clause
: tPORT tLPAREN interface_list tRPAREN tSEMI { $$ = $3; }
| /* empty */ { $$ = NULL; }
;

arch_body
: tARCHITECTURE id tOF id tIS arch_decl_part tBEGIN arch_stmt_part
  tEND opt_arch_token opt_id tSEMI
  {
     $$ = tree_new(T_ARCH);
     tree_set_ident($$, $2);
     tree_set_ident2($$, $4);
     tree_set_loc($$, &@$);
     copy_trees($6, tree_add_decl, $$);
     copy_trees($8, tree_add_stmt, $$);

     if ($11 != NULL && $11 != $2) {
        parse_error(&@11, "%s does not match architecture name %s",
                    istr($11), istr($2));
     }
  }
;

arch_decl_part
: block_decl_item arch_decl_part
  {
     $$ = $1;
     tree_list_concat(&$$, $2);
  }
| /* empty */
  {
     $$ = NULL;
  }
;

block_decl_item
: signal_decl
| type_decl
| subtype_decl
| constant_decl
| subprogram_decl
/* | subprogram_body
   | shared_variable_declaration
   | file_declaration
   | alias_declaration
   | component_declaration
   | attribute_declaration
   | attribute_specification
   | configuration_specification
   | disconnection_specification
   | use_clause
   | group_template_declaration
   | group_declaration */
;

signal_decl
: tSIGNAL id_list tCOLON subtype_indication
  /* signal_kind */ opt_static_expr tSEMI
  {
     $$ = NULL;
     for (id_list_t *it = $2; it != NULL; it = it->next) {
        tree_t t = tree_new(T_SIGNAL_DECL);
        tree_set_ident(t, it->id);
        tree_set_type(t, $4);
        tree_set_value(t, $5);
        tree_set_loc(t, &@$);

        tree_list_append(&$$, t);
     }

     id_list_free($2);
  }
;

constant_decl
: tCONSTANT id_list tCOLON subtype_indication opt_static_expr tSEMI
  {
     $$ = NULL;
     for (id_list_t *it = $2; it != NULL; it = it->next) {
        tree_t t = tree_new(T_CONST_DECL);
        tree_set_ident(t, it->id);
        tree_set_type(t, $4);
        tree_set_value(t, $5);
        tree_set_loc(t, &@$);

        tree_list_append(&$$, t);
     }

     id_list_free($2);
  }
;

arch_stmt_part
: conc_stmt arch_stmt_part
  {
     $$ = $2;
     tree_list_prepend(&$$, $1);
  }
| /* empty */
  {
     $$ = NULL;
  }
;

opt_arch_token : tARCHITECTURE | /* empty */ ;

interface_list
: interface_decl
  {
     $$ = $1;
  }
| interface_decl tSEMI interface_list
  {
     $$ = $1;
     tree_list_concat(&$$, $3);
  }
;

interface_decl
: interface_object_decl
  /* | interface_type_declaration
     | interface_subprogram_declaration
     | interface_package_declaration */
;

interface_object_decl
: interface_signal_decl
  /* | interface_constant_declaration
     | interface_variable_declaration
     | interface_file_declaration */
;

interface_signal_decl
: opt_signal_token id_list tCOLON opt_mode subtype_indication
  /* opt_bus_token */ opt_static_expr
  {
     $$ = NULL;
     for (id_list_t *it = $2; it != NULL; it = it->next) {
        tree_t t = tree_new(T_PORT_DECL);
        tree_set_ident(t, it->id);
        tree_set_port_mode(t, $4);
        tree_set_type(t, $5);
        tree_set_value(t, $6);
        tree_set_loc(t, &@2);

        tree_list_append(&$$, t);
     }

     id_list_free($2);
  }
;

opt_static_expr
: tASSIGN expr
  {
     $$ = $2;
  }
| /* empty */
  { $$ = NULL; }
;

opt_signal_token : tSIGNAL | /* empty */ ;

opt_mode
: tIN { $$ = PORT_IN; }
| tOUT { $$ = PORT_OUT; }
| tINOUT { $$ = PORT_INOUT; }
| tBUFFER { $$ = PORT_BUFFER; }
| /* empty */ { $$ = PORT_IN; }
;

subtype_indication
: type_mark { $$ = $1; }
| type_mark constraint
  {
     $$ = type_new(T_SUBTYPE);
     type_set_base($$, $1);
     type_add_dim($$, $2);
  }
;

constraint : range_constraint | index_constraint ;

conc_stmt
: id tCOLON conc_stmt_without_label
  {
     $$ = $3;
     if (tree_has_ident($$) && tree_ident($$) != $1)
        parse_error(&@1, "%s does not match process name %s",
                    istr(tree_ident($$)), istr($1));
     else
        tree_set_ident($$, $1);
  }
| conc_stmt_without_label
  {
     $$ = $1;
     if (tree_has_ident($$))
        parse_error(&@$, "process does not have a label");
     else
        tree_set_ident($$, ident_uniq("_proc"));
  }
;

conc_stmt_without_label
: process_stmt | comp_instance_stmt
  /* | block_statement
     | process_statement
     | concurrent_procedure_call_statement
     | concurrent_assertion_statement
     | concurrent_signal_assignment_statement
     | generate_statement */
;

comp_instance_stmt
: id generic_map port_map tSEMI
  {
     $$ = tree_new(T_INSTANCE);
     tree_set_loc($$, &@$);
     tree_set_ident2($$, $1);
     copy_params($3, tree_add_param, $$);
     copy_params($2, tree_add_genmap, $$);
  }
| tCOMPONENT selected_id generic_map port_map tSEMI
  {
     $$ = tree_new(T_INSTANCE);
     tree_set_loc($$, &@$);
     tree_set_ident2($$, $2);
     copy_params($4, tree_add_param, $$);
     copy_params($3, tree_add_genmap, $$);
  }
| tENTITY selected_id generic_map port_map tSEMI
  {
     $$ = tree_new(T_INSTANCE);
     tree_set_loc($$, &@$);
     tree_set_ident2($$, $2);
     copy_params($4, tree_add_param, $$);
     copy_params($3, tree_add_genmap, $$);
  }
| tENTITY selected_id tLPAREN id tRPAREN generic_map port_map tSEMI
  {
     $$ = tree_new(T_INSTANCE);
     tree_set_loc($$, &@$);
     tree_set_ident2($$, ident_prefix($2, $4, '-'));
     copy_params($7, tree_add_param, $$);
     copy_params($6, tree_add_genmap, $$);
  }
| tCONFIGURATION selected_id generic_map port_map tSEMI
  {
     $$ = tree_new(T_INSTANCE);
     tree_set_loc($$, &@$);
     tree_set_ident2($$, $2);
     copy_params($4, tree_add_param, $$);
     copy_params($3, tree_add_genmap, $$);
  }
;

generic_map
: tGENERIC tMAP tLPAREN param_list tRPAREN { $$ = $4; }
| /* empty */ { $$ = NULL; }
;

port_map
: tPORT tMAP tLPAREN  param_list tRPAREN { $$ = $4; }
| /* empty */ { $$ = NULL; }
;

process_stmt
: /* [ postponed ] */ tPROCESS process_sensitivity_clause opt_is
  process_decl_part tBEGIN seq_stmt_list tEND /* [ postponed ] */
  tPROCESS opt_id tSEMI
  {
     $$ = tree_new(T_PROCESS);
     tree_set_loc($$, &@2);
     copy_trees($4, tree_add_decl, $$);
     copy_trees($6, tree_add_stmt, $$);
     copy_trees($2, tree_add_trigger, $$);
     if ($9 != NULL)
        tree_set_ident($$, $9);
  }
;

process_sensitivity_clause
: tLPAREN name_list tRPAREN { $$ = $2; }
| /* empty */ { $$ = NULL; }
;

opt_is : tIS | /* empty */ ;

process_decl_part
: process_decl_item process_decl_part
  {
     $$ = $1;
     tree_list_concat(&$$, $2);
  }
| /* empty */
  {
     $$ = NULL;
  }
;

process_decl_item
: variable_decl
| type_decl
| subtype_decl
| constant_decl
| subprogram_decl
  /* | subprogram_body
     | file_declaration
     | alias_declaration
     | attribute_declaration
     | attribute_specification
     | use_clause
     | group_template_declaration
     | group_declaration
  */
;

subprogram_decl
: /* procedure designator [ ( formal_parameter_list ) ]  | */
  func_type func_name formal_param_list tRETURN type_mark tSEMI
  {
     type_t t = type_new(T_FUNC);
     type_set_ident(t, $2);
     type_set_result(t, $5);

     tree_t f = tree_new(T_FUNC_DECL);
     tree_set_loc(f, &@$);
     tree_set_ident(f, $2);
     tree_set_type(f, t);

     for (tree_list_t *it = $3; it != NULL; it = it->next)
        tree_add_port(f, it->value);

     tree_list_free($3);

     $$ = NULL;
     tree_list_append(&$$, f);
  }
;

func_type : tPURE tFUNCTION | tIMPURE tFUNCTION | tFUNCTION ;

func_name
: id
| tSTRING
  {
     $$ = ident_new(lvals.sval);
     free(lvals.sval);
  }
;

formal_param_list
: tLPAREN interface_list tRPAREN { $$ = $2; }
| /* empty */ { $$ = NULL; }
;

variable_decl
: /* [ shared ] */ tVARIABLE id_list tCOLON subtype_indication
  opt_static_expr tSEMI
  {
     $$ = NULL;
     for (id_list_t *it = $2; it != NULL; it = it->next) {
        tree_t t = tree_new(T_VAR_DECL);
        tree_set_ident(t, it->id);
        tree_set_type(t, $4);
        tree_set_value(t, $5);
        tree_set_loc(t, &@$);

        tree_list_append(&$$, t);
     }

     id_list_free($2);
  }

seq_stmt_list
: seq_stmt seq_stmt_list
  {
     $$ = $2;
     tree_list_prepend(&$$, $1);
  }
| /* empty */
  {
     $$ = NULL;
  }
;

seq_stmt
: id tCOLON seq_stmt_without_label
  {
     $$ = $3;
     tree_set_ident($$, $1);
  }
| seq_stmt_without_label
  {
     $$ = $1;
     tree_set_ident($$, ident_uniq("s"));
  }
;

seq_stmt_without_label
: tWAIT sensitivity_clause /* [ condition_clause ] */
  timeout_clause tSEMI
  {
     $$ = tree_new(T_WAIT);
     tree_set_loc($$, &@$);
     if ($3 != NULL)
        tree_set_delay($$, $3);
     copy_trees($2, tree_add_trigger, $$);
  }
| target tASSIGN expr tSEMI
  {
     $$ = tree_new(T_VAR_ASSIGN);
     tree_set_target($$, $1);
     tree_set_value($$, $3);
     tree_set_loc($$, &@$);
  }
| target tLE /* [ delay_mechanism ] */ waveform tSEMI
  {
     $$ = tree_new(T_SIGNAL_ASSIGN);
     tree_set_target($$, $1);
     tree_set_value($$, $3);
     tree_set_loc($$, &@$);
  }
| tASSERT expr report severity tSEMI
  {
     if ($4 == NULL) {
        $4 = tree_new(T_REF);
        tree_set_ident($4, ident_new("ERROR"));
     }

     $$ = tree_new(T_ASSERT);
     tree_set_loc($$, &@$);
     tree_set_value($$, $2);
     tree_set_severity($$, $4);
     tree_set_message($$, $3);
  }
| tREPORT expr severity tSEMI
  {
     tree_t false_ref = tree_new(T_REF);
     tree_set_ident(false_ref, ident_new("FALSE"));

     if ($3 == NULL) {
        $3 = tree_new(T_REF);
        tree_set_ident($3, ident_new("NOTE"));
     }

     $$ = tree_new(T_ASSERT);
     tree_set_loc($$, &@$);
     tree_set_value($$, false_ref);
     tree_set_severity($$, $3);
     tree_set_message($$, $2);
     tree_add_attr_int($$, ident_new("is_report"), 1);
  }
| tIF expr tTHEN seq_stmt_list tEND tIF opt_id tSEMI
  {
     $$ = tree_new(T_IF);
     tree_set_loc($$, &@$);
     tree_set_value($$, $2);
     copy_trees($4, tree_add_stmt, $$);
  }
| tIF expr tTHEN seq_stmt_list tELSIF elsif_list
  {
     $$ = tree_new(T_IF);
     tree_set_loc($$, &@$);
     tree_set_value($$, $2);
     copy_trees($4, tree_add_stmt, $$);
     tree_add_else_stmt($$, $6);
  }
| tIF expr tTHEN seq_stmt_list tELSE seq_stmt_list
  tEND tIF opt_id tSEMI
  {
     $$ = tree_new(T_IF);
     tree_set_loc($$, &@$);
     tree_set_value($$, $2);
     copy_trees($4, tree_add_stmt, $$);
     copy_trees($6, tree_add_else_stmt, $$);
  }
| tNULL tSEMI
  {
     $$ = tree_new(T_NULL);
     tree_set_loc($$, &@$);
  }
/* | procedure_call_statement
   | case_statement
   | loop_statement
   | next_statement
   | exit_statement
   | return_statement */
;

elsif_list
: expr tTHEN seq_stmt_list tEND tIF opt_id tSEMI
  {
     $$ = tree_new(T_IF);
     tree_set_ident($$, ident_uniq("elsif"));
     tree_set_loc($$, &@$);
     tree_set_value($$, $1);
     copy_trees($3, tree_add_stmt, $$);
  }
| expr tTHEN seq_stmt_list tELSE seq_stmt_list
  tEND tIF opt_id tSEMI
  {
     $$ = tree_new(T_IF);
     tree_set_ident($$, ident_uniq("elsif"));
     tree_set_loc($$, &@$);
     tree_set_value($$, $1);
     copy_trees($3, tree_add_stmt, $$);
     copy_trees($5, tree_add_else_stmt, $$);
  }
| expr tTHEN seq_stmt_list tELSIF elsif_list
  {
     $$ = tree_new(T_IF);
     tree_set_ident($$, ident_uniq("elsif"));
     tree_set_loc($$, &@$);
     tree_set_value($$, $1);
     copy_trees($3, tree_add_stmt, $$);
     tree_add_else_stmt($$, $5);
  }
;

report
: /* empty */
  {
     $$ = str_to_agg("Assertion violation.", NULL);
  }
| tREPORT expr { $$ = $2; }
;

severity
: /* empty */ { $$ = NULL; }
| tSEVERITY expr { $$ = $2; }
;

target
: name
  {
     // A target cannot be a function call so resolve ambiguity here
     if (tree_kind($1) == T_FCALL)
        tree_change_kind($1, T_ARRAY_REF);
     $$ = $1;
  }
;

waveform
: waveform_element
| waveform_element tCOMMA waveform
| tUNAFFECTED { $$ = NULL; }
;

waveform_element
: expr /* [ after time_expression ] */
| tNULL /* [ after time_expression ] */ { $$ = NULL; }
;

timeout_clause
: tFOR expr { $$ = $2; }
| /* empty */ { $$ = NULL; }
;

sensitivity_clause
: tON name_list { $$ = $2; }
| /* empty */ { $$ = NULL; }
;

subtype_decl
: tSUBTYPE id tIS subtype_indication tSEMI
  {
     type_t sub = $4;
     if (type_kind(sub) != T_SUBTYPE) {
        // Case where subtype_indication did not impose any
        // constraint so we must create the subtype object here
        sub = type_new(T_SUBTYPE);
        type_set_base(sub, $4);
     }
     type_set_ident(sub, $2);

     tree_t t = tree_new(T_TYPE_DECL);
     tree_set_ident(t, $2);
     tree_set_type(t, sub);
     tree_set_loc(t, &@$);

     $$ = NULL;
     tree_list_append(&$$, t);
  }
| tSUBTYPE id tIS id subtype_indication tSEMI
  {
     tree_t r = tree_new(T_REF);
     tree_set_loc(r, &@4);
     tree_set_ident(r, $4);

     type_t sub = type_new(T_SUBTYPE);
     type_set_base(sub, $5);
     type_set_ident(sub, $2);
     type_set_resolution(sub, r);

     tree_t t = tree_new(T_TYPE_DECL);
     tree_set_ident(t, $2);
     tree_set_type(t, sub);
     tree_set_loc(t, &@$);

     $$ = NULL;
     tree_list_append(&$$, t);
  }
;

type_decl
: tTYPE id tIS type_def tSEMI
  {
     tree_t t = tree_new(T_TYPE_DECL);
     tree_set_ident(t, $2);
     tree_set_type(t, $4);
     tree_set_loc(t, &@$);

     type_set_ident($4, $2);

     $$ = NULL;
     tree_list_append(&$$, t);
  }
| tTYPE id tSEMI
  {
     type_t i = type_new(T_INCOMPLETE);
     type_set_ident(i, $2);

     tree_t t = tree_new(T_TYPE_DECL);
     tree_set_ident(t, $2);
     tree_set_type(t, i);
     tree_set_loc(t, &@$);

     $$ = NULL;
     tree_list_append(&$$, t);
  }
;

type_def
: scalar_type_def
| array_type_def
  /* | record_type_def
     | access_type_definition
     | file_type_definition */
;

array_type_def
: unconstrained_array_def
| constrained_array_def
;

constrained_array_def
: tARRAY index_constraint tOF subtype_indication
  {
     $$ = type_new(T_CARRAY);
     type_set_base($$, $4);
     type_add_dim($$, $2);
  }
;

index_constraint
: tLPAREN range /* { , range } */ tRPAREN { $$ = $2; }
;

unconstrained_array_def
: tARRAY tLPAREN index_subtype_def /* { , index_subtype_def } */
  tRPAREN tOF subtype_indication
  {
     $$ = type_new(T_UARRAY);
     type_set_base($$, $6);
     type_add_index_constr($$, $3);
  }
;

index_subtype_def : type_mark tRANGE tBOX { $$ = $1; } ;

scalar_type_def
: integer_type_def
| physical_type_def
| enum_type_def
  /* | floating_type_definition */
;

integer_type_def
: range_constraint
  {
     $$ = type_new(T_INTEGER);
     type_add_dim($$, $1);
  }
;

enum_type_def
: tLPAREN enum_lit_list tRPAREN
  {
     $$ = type_new(T_ENUM);

     unsigned pos = 0;
     for (tree_list_t *it = $2; it != NULL; it = it->next) {
        tree_set_type(it->value, $$);
        tree_set_pos(it->value, pos++);
        type_enum_add_literal($$, it->value);
     }
     tree_list_free($2);
  }
;

enum_lit_list
: id
  {
     tree_t t = tree_new(T_ENUM_LIT);
     tree_set_ident(t, $1);
     tree_set_loc(t, &@$);

     $$ = NULL;
     tree_list_append(&$$, t);
  }
| id tCOMMA enum_lit_list
  {
     tree_t t = tree_new(T_ENUM_LIT);
     tree_set_ident(t, $1);
     tree_set_loc(t, &@1);

     $$ = $3;
     tree_list_prepend(&$$, t);
  }
;

physical_type_def
: range_constraint tUNITS base_unit_decl secondary_unit_decls tEND
  tUNITS opt_id
  {
     $$ = type_new(T_PHYSICAL);
     type_add_dim($$, $1);
     type_add_unit($$, $3);

     for (unit_list_t *it = $4; it != NULL; it = it->next)
        type_add_unit($$, it->unit);

     unit_list_free($4);
  }
;

base_unit_decl
: id tSEMI
  {
     $$.multiplier = tree_new(T_LITERAL);
     literal_t l = { { .i = 1 }, .kind = L_INT };
     tree_set_literal($$.multiplier, l);
     $$.name = $1;
  }
;

secondary_unit_decls
: /* empty */ { $$ = NULL; }
| id tEQ physical_literal tSEMI secondary_unit_decls
  {
     unit_t u = {
        .name       = $1,
        .multiplier = $3
     };

     $$ = unit_list_add($5, u);
  }
;

range_constraint : tRANGE range { $$ = $2; } ;

range
: expr tTO expr
  {
     $$.left  = $1;
     $$.right = $3;
     $$.kind  = RANGE_TO;
  }
| expr tDOWNTO expr
  {
     $$.left  = $1;
     $$.right = $3;
     $$.kind  = RANGE_DOWNTO;
  }
  /* | attribute_name */
;

expr
: expr tAND expr { $$ = build_expr2("and", $1, $3, &@$); }
| expr tOR expr { $$ = build_expr2("or", $1, $3, &@$); }
| expr tNAND expr { $$ = build_expr2("nand", $1, $3, &@$); }
| expr tNOR expr { $$ = build_expr2("nor", $1, $3, &@$); }
| expr tXOR expr { $$ = build_expr2("xor", $1, $3, &@$); }
| expr tXNOR expr { $$ = build_expr2("xnor", $1, $3, &@$); }
| expr tEQ expr { $$ = build_expr2("=", $1, $3, &@$); }
| expr tNEQ expr { $$ = build_expr2("/=", $1, $3, &@$); }
| expr tLT expr { $$ = build_expr2("<", $1, $3, &@$); }
| expr tLE expr { $$ = build_expr2("<=", $1, $3, &@$); }
| expr tGT expr { $$ = build_expr2(">", $1, $3, &@$); }
| expr tGE expr { $$ = build_expr2(">=", $1, $3, &@$); }
| expr tSLL expr { $$ = build_expr2("sll", $1, $3, &@$); }
| expr tSRL expr { $$ = build_expr2("srl", $1, $3, &@$); }
| expr tSLA expr { $$ = build_expr2("sla", $1, $3, &@$); }
| expr tSRA expr { $$ = build_expr2("sra", $1, $3, &@$); }
| expr tROL expr { $$ = build_expr2("rol", $1, $3, &@$); }
| expr tROR expr { $$ = build_expr2("ror", $1, $3, &@$); }
| expr tPLUS expr { $$ = build_expr2("+", $1, $3, &@$); }
| expr tMINUS expr { $$ = build_expr2("-", $1, $3, &@$); }
| expr tAMP expr { $$ = build_expr2("&", $1, $3, &@$); }
| expr tTIMES expr { $$ = build_expr2("*", $1, $3, &@$); }
| expr tOVER expr { $$ = build_expr2("/", $1, $3, &@$); }
| expr tMOD expr { $$ = build_expr2("mod", $1, $3, &@$); }
| expr tREM expr { $$ = build_expr2("rem", $1, $3, &@$); }
| expr tPOWER expr { $$ = build_expr2("**", $1, $3, &@$); }
| tNOT expr { $$ = build_expr1("not", $2, &@$); }
| tABS expr { $$ = build_expr1("abs", $2, &@$); }
| tMINUS expr { $$ = build_expr1("-", $2, &@$); }
| tPLUS expr { $$ = build_expr1("+", $2, &@$); }
| name
| literal
| selected_id tTICK tLPAREN expr tRPAREN
  {
     $$ = tree_new(T_QUALIFIED);
     tree_set_ident($$, $1);
     tree_set_value($$, $4);
     tree_set_loc($$, &@$);
  }
| aggregate
/*
  | type_conversion
  | allocator
*/
;

param_list
: expr
  {
     param_t p = { .kind = P_POS, .value = $1 };
     $$ = param_list_add(NULL, p);
  }
| expr tCOMMA param_list
  {
     param_t p = { .kind = P_POS, .value = $1 };
     $$ = param_list_add($3, p);
  }
| id tASSOC expr
  {
     param_t p = { .kind = P_NAMED, .value = $3 };
     p.name = $1;
     $$ = param_list_add(NULL, p);
  }
| id tASSOC expr tCOMMA param_list
  {
     param_t p = { .kind = P_NAMED, .value = $3 };
     p.name = $1;
     $$ = param_list_add($5, p);
  }
;

aggregate
: tLPAREN element_assoc_list tRPAREN
  {
     // The grammar is ambiguous between an aggregate with a
     // single positional element and a parenthesised expression
     if ($2->next == NULL && $2->assoc.kind == A_POS) {
        $$ = $2->assoc.value;
     }
     else {
        $$ = tree_new(T_AGGREGATE);
        tree_set_loc($$, &@$);

        for (assoc_list_t *it = $2; it != NULL; it = it->next)
           tree_add_assoc($$, it->assoc);
     }

     assoc_list_free($2);
  }
;

element_assoc_list
: element_assoc
  {
     $$ = assoc_list_add(NULL, $1);
  }
| element_assoc tCOMMA element_assoc_list
  {
     $$ = assoc_list_add($3, $1);
  }
;

element_assoc
: expr
  {
     $$.kind  = A_POS;
     $$.value = $1;
  }
| expr tASSOC expr
  {
     $$.kind  = A_NAMED;
     $$.name  = $1;
     $$.value = $3;
  }
| range tASSOC expr
  {
     $$.kind  = A_RANGE;
     $$.range = $1;
     $$.value = $3;
  }
| tOTHERS tASSOC expr
  {
     $$.kind  = A_OTHERS;
     $$.value = $3;
  }
;

literal
: numeric_literal
| string_literal
/* | bit_string_literal
   | null
*/
;

string_literal
: tSTRING
  {
     size_t len = strlen(lvals.sval);
     $$ = str_to_agg(lvals.sval + 1, lvals.sval + len - 1);
     tree_set_loc($$, &@$);

     free(lvals.sval);
  }
;

numeric_literal : abstract_literal | physical_literal ;

abstract_literal
: tINT
  {
     $$ = tree_new(T_LITERAL);
     tree_set_loc($$, &@$);
     literal_t l = { { .i = lvals.ival }, .kind = L_INT };
     tree_set_literal($$, l);
  }
/* | tFLOAT */
;

physical_literal
: abstract_literal id
  {
     tree_t unit = tree_new(T_REF);
     tree_set_ident(unit, $2);
     tree_set_loc(unit, &@2);

     $$ = tree_new(T_FCALL);
     tree_set_loc($$, &@$);
     tree_set_ident($$, ident_new("*"));
     param_t left = { .kind = P_POS, .value = $1 };
     tree_add_param($$, left);
     param_t right = { .kind = P_POS, .value = unit };
     tree_add_param($$, right);
  }
;

type_mark
: selected_id
  {
     $$ = type_new(T_UNRESOLVED);
     type_set_ident($$, $1);
  }
;

name
: selected_id
  {
     $$ = tree_new(T_REF);
     tree_set_ident($$, $1);
     tree_set_loc($$, &@$);
  }
| selected_id tTICK id
  {
     $$ = tree_new(T_ATTR_REF);
     tree_set_ident($$, $1);
     tree_set_ident2($$, $3);
     tree_set_loc($$, &@$);
  }
| name tLPAREN param_list tRPAREN
  {
     // This is ambiguous between an array reference and a function
     // call: in the case where $1 is a simple reference assume it
     // is a function call for now and the semantic checker will
     // fix things up later. We stash the value $1 in the tree
     // anyway to make changing the kind easier.

     if (tree_kind($1) == T_ATTR_REF) {
        $$ = $1;
     }
     else {
        $$ = tree_new(T_ARRAY_REF);
        tree_set_value($$, $1);

        if (tree_kind($1) == T_REF) {
           tree_change_kind($$, T_FCALL);
           tree_set_ident($$, tree_ident($1));
        }
     }

     tree_set_loc($$, &@$);
     copy_params($3, tree_add_param, $$);
  }
| name tLPAREN range tRPAREN
  {
     $$ = tree_new(T_ARRAY_SLICE);
     tree_set_value($$, $1);
     tree_set_range($$, $3);
     tree_set_loc($$, &@$);
  }
  /* | operator_symbol
     | character_literal
     | indexed_name
     | external_name */
;

name_list
: name
  {
     $$ = NULL;
     tree_list_append(&$$, $1);
  }
| name tCOMMA name_list
  {
     $$ = $3;
     tree_list_prepend(&$$, $1);
  }
;

%%

static void tree_list_append(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = NULL;
   new->value = t;

   tree_list_concat(l, new);
}

static void tree_list_prepend(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = *l;
   new->value = t;

   *l = new;
}

static void tree_list_concat(struct tree_list **a, struct tree_list *b)
{
   if (*a == NULL)
      *a = b;
   else {
      struct tree_list *it;
      for (it = *a; it->next != NULL; it = it->next)
         ;
      it->next = b;
   }
}

static void tree_list_free(struct tree_list *l)
{
   while (l != NULL) {
      struct tree_list *next = l->next;
      free(l);
      l = next;
   }
}

static void copy_trees(tree_list_t *from,
                       void (*copy_fn)(tree_t t, tree_t d),
                       tree_t to)
{
   for (; from != NULL; from = from->next)
      (*copy_fn)(to, from->value);
   tree_list_free(from);
}

static void copy_params(param_list_t *from,
                        void (*copy_fn)(tree_t t, param_t p),
                        tree_t to)
{
   for (; from != NULL; from = from->next)
      (*copy_fn)(to, from->param);
   param_list_free(from);
}

static id_list_t *id_list_add(id_list_t *list, ident_t id)
{
   id_list_t *new = xmalloc(sizeof(id_list_t));
   new->next = list;
   new->id   = id;
   return new;
}

static id_list_t *id_list_append(id_list_t *a, id_list_t *b)
{
   if (a == NULL)
      return b;

   while (a->next)
      a = a->next;
   a->next = b;

   return a;
}

static void id_list_free(id_list_t *list)
{
   while (list != NULL) {
      id_list_t *next = list->next;
      free(list);
      list = next;
   }
}

static unit_list_t *unit_list_add(unit_list_t *list, unit_t u)
{
   unit_list_t *new = xmalloc(sizeof(unit_list_t));
   new->next = list;
   new->unit = u;
   return new;
}

static void unit_list_free(unit_list_t *list)
{
   while (list != NULL) {
      unit_list_t *next = list->next;
      free(list);
      list = next;
   }
}

static param_list_t *param_list_add(param_list_t *list, param_t a)
{
   param_list_t *new = xmalloc(sizeof(param_list_t));
   new->next  = list;
   new->param = a;
   return new;
}

static void param_list_free(param_list_t *list)
{
   while (list != NULL) {
      param_list_t *next = list->next;
      free(list);
      list = next;
   }
}

static assoc_list_t *assoc_list_add(assoc_list_t *list, assoc_t a)
{
   assoc_list_t *new = xmalloc(sizeof(assoc_list_t));
   new->next  = list;
   new->assoc = a;
   return new;
}

static void assoc_list_free(assoc_list_t *list)
{
   while (list != NULL) {
      assoc_list_t *next = list->next;
      free(list);
      list = next;
   }
}

static tree_t build_expr1(const char *fn, tree_t arg,
                          const struct YYLTYPE *loc)
{
   tree_t t = tree_new(T_FCALL);
   tree_set_ident(t, ident_new(fn));
   param_t parg = { .kind = P_POS, .value = arg };
   tree_add_param(t, parg);
   tree_set_loc(t, loc);

   return t;
}

static tree_t build_expr2(const char *fn, tree_t left, tree_t right,
                          const struct YYLTYPE *loc)
{
   tree_t t = tree_new(T_FCALL);
   tree_set_ident(t, ident_new(fn));
   param_t pleft = { .kind = P_POS, .value = left };
   tree_add_param(t, pleft);
   param_t pright = { .kind = P_POS, .value = right };
   tree_add_param(t, pright);
   tree_set_loc(t, loc);

   return t;
}

static tree_t str_to_agg(const char *start, const char *end)
{
   tree_t t = tree_new(T_AGGREGATE);

   for (const char *p = start; *p != '\0' && p != end; p++) {
      const char ch[] = { '\'', *p, '\'', '\0' };

      tree_t ref = tree_new(T_REF);
      tree_set_ident(ref, ident_new(ch));

      assoc_t a;
      a.kind  = A_POS;
      a.value = ref;

      tree_add_assoc(t, a);
   }

   return t;
}

static void parse_error(const loc_t *loc, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   fprintf(stderr, "%s:%d: ",  loc->file, loc->first_line);
   vfprintf(stderr, fmt, ap);
   fprintf(stderr, "\n");
   fmt_loc(stderr, loc);
   va_end(ap);

   n_errors++;
}

static void yyerror(const char *s)
{
   parse_error(&yylloc, "%s", s);
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

   yylloc.first_line   = n_row;
   yylloc.first_column = n_token_start;
   yylloc.last_line    = n_row;
   yylloc.last_column  = n_token_start + n_token_length - 1;
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

bool input_from_file(const char *file)
{
   int fd = open(file, O_RDONLY);
   if (fd < 0) {
      perror(file);
      return false;
   }

   struct stat buf;
   if (fstat(fd, &buf) != 0) {
      perror("fstat");
      return false;
   }

   file_sz = buf.st_size;

   file_start = mmap(NULL, file_sz, PROT_READ, MAP_PRIVATE, fd, 0);
   if (file_start == MAP_FAILED) {
      perror("mmap");
      return false;
   }

   read_ptr         = file_start;
   last_was_newline = true;
   perm_file_name   = strdup(file);

   return true;
}

tree_t parse(void)
{
   root = NULL;

   if (yyparse())
      return NULL;
   else
      return root;
}

int parse_errors(void)
{
   return n_errors;
}
