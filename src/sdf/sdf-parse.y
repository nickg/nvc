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
#include "object.h"
#include "scan.h"
#include "hash.h"
#include "sdf/sdf-node.h"
#include "sdf/sdf-phase.h"

#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>

#define YYLTYPE loc_t
#define YYSTYPE yylval_t
#define YYTOKENTYPE token_t

#define YYEMPTY -2
#define YYEOF tEOF
#define YYerror -3
#define YYUNDEF tERROR

// #define YYDEBUG 1

// Do not use
// #define yylex processed_yylex

static void yyerror(const char *s);

static sdf_file_t *ctx;

#define YYLLOC_DEFAULT(Current, Rhs, N) {                               \
   if (N) {                                                             \
      (Current) = get_loc(YYRHSLOC(Rhs, 1).first_line,                  \
                          YYRHSLOC(Rhs, 1).first_column,                \
                          YYRHSLOC(Rhs, N).first_line +                 \
                          YYRHSLOC(Rhs, N).line_delta,                  \
                          YYRHSLOC(Rhs, N).first_column +               \
                          YYRHSLOC(Rhs, N).column_delta,                \
                          YYRHSLOC(Rhs, N).file_ref);                   \
   }                                                                    \
   else {                                                               \
      (Current) = YYRHSLOC(Rhs, 0);                                     \
   }                                                                    \
 }

#define EPSILON 0.0000000000001

static bool doubles_equal(double a, double b)
{
   if (fabs(a - b) < EPSILON)
      return true;
   return false;
}

static void check_delval_list_len(sdf_node_t del)
{
   assert (sdf_kind(del) == S_DELAY);

   unsigned len = sdf_values(del);
   if (len > 12)
      yyerror("'delval_list' shall have at most 12 'delval' entries");
}

static void check_header_item(sdf_header_item_kind_t kind)
{
   if (ctx->header_mask & kind)
      yyerror("Duplicit header item");
   else if (ctx->header_mask > kind)
      yyerror("Invalid header item order. SDF header items shall have strict order:\n"
              "\tSDFVERSION, DESIGN, DATE, VENDOR, PROGRAM, VERSION, DIVIDER, VOLTAGE, "
              "PROCESS, TEMPERATURE, TIMESCALE");

   ctx->header_mask |= kind;
}

static void insert_sdf_cell(sdf_node_t cell)
{
   assert(sdf_kind(cell) = S_CELL);

   sdf_add_cell(ctx->root, cell);

   const char *cell_instance = istr(sdf_ident2(cell));
   const char *cell_type = istr(sdf_ident(cell));

   shash_t *hash = (!strcmp(cell_instance, "*")) ? ctx->nmap : ctx->hmap;
   ptr_list_t *l = (ptr_list_t*)(shash_get(hash, cell_type));

   if (l == NULL) {
      l = (ptr_list_t*) xcalloc(sizeof(ptr_list_t));
      shash_put(hash, cell_type, l);
   }

   // TODO: Here it might be better to store only index of the cell in
   //       sdf_cells(ctx->root). At the time of adding the cell, the
   //       index is known. Then, if the root sdf_node is serialized together
   //       with the hash table, we are able to deserialize back the hash
   //       table with valid data pointing into the list of cells!
   list_add(l, cell);
}

%}

//%define api.prefix {ysdf_}


%type   <sdf>              delay_file

%type   <int>              sdf_header_qstring
%type   <kind>             qstring_header_entry

%type   <sdf>              sdf_cell
%type   <sdf>              sdf_cell_head
%type   <sdf>              timescale_number
%type   <ident>            timescale_unit
%type   <ident>            celltype
%type   <ident>            cell_instance
%type   <sdf>              tchk_2_port_1_val
%type   <kind>             tchk_2_port_1_val_kind
%type   <sdf>              tchk_merged
%type   <kind>             tchk_merged_kind
%type   <sdf>              tchk_1_port_1_val
%type   <kind>             tchk_1_port_1_val_kind
%type   <sdf>              tchk_def
%type   <sdf>              skew_timing_check
%type   <sdf>              bidirectskew_timing_check
%type   <sdf>              nochange_timing_check
%type   <ident>            identifier
%type   <ident>            hierarchical_identifier
%type   <sdf>              del_def
%type   <sdf>              iopath_def
%type   <sdf>              cond_def
%type   <sdf>              condelse_def
%type   <sdf>              port_def
%type   <sdf>              conditional_port_expr
%type   <sdf>              interconnect_def
%type   <sdf>              netdelay_def
%type   <sdf>              device_def
%type   <sdf>              port_spec
%type   <sdf>              port_tchk
%type   <sdf>              port_edge
%type   <sdf>              port_instance
%type   <sdf>              port
%type   <sdf>              scalar_port
%type   <sdf>              bus_port
%type   <sdf>              port_name
%type   <sdf>              delval
%type   <sdf>              rvalue
%type   <sdf>              value
%type   <kind>             edge_identifier
%type   <sdf>              signed_real_or_rtripple
%type   <sdf>              real_or_tripple
%type   <sdf>              signed_real_number
%type   <sdf>              real_number
%type   <sdf>              rtripple
%type   <sdf>              rtripple_tail_2
%type   <sdf>              rtripple_tail_1
%type   <sdf>              tripple
%type   <sdf>              tripple_tail_2
%type   <sdf>              tripple_tail_1

%type   <i64>              integer
%type   <real>             real
%type   <kind>             hchar



%token                  tDELAYFILE 500 "delayfile"
%token                  tSDFVERSION 501 "sdfversion"
%token                  tDESIGN 502 "design"
%token                  tDATE 503 "date"
%token                  tVENDOR 504 "vendor"
%token                  tPROGRAM 505 "program"
%token                  tVERSION 506 "version"
%token                  tDIVIDER 507 "divider"
%token                  tVOLTAGE 508 "voltage"
%token                  tPROCESS 223 "process"
%token                  tTEMPERATURE 510 "temperature"
%token                  tTIMESCALE 399 "timescale"
%token                  tCELL 512 "cell"
%token                  tCELLTYPE 513 "celltype"
%token                  tINSTANCE 514 "instance"
%token                  tDELAY 515 "delay"
%token                  tLABEL 268 "label"
%token                  tTIMINGCHECK 516 "timingcheck"
%token                  tTIMINGENV 517 "timingenv"
%token                  tEOF 0 "end of file"
%token                  tINCREMENT 554 "increment"
%token                  tPATHPULSE 519 "pathpulse"
%token                  tPATHPULSEP 520 "pathpulsepercent"
%token                  tABSOLUTE 555 "absolute"
%token                  tIOPATH 522 "iopath"
%token                  tPOSEDGE 355 "posedge"
%token                  tNEGEDGE 356 "negedge"
%token                  tSETUP 529 "setup"
%token                  tHOLD 530 "hold"
%token                  tSETUPHOLD 531 "setuphold"
%token                  tRECOVERY 532 "recovery"
%token                  tREMOVAL 533 "removal"
%token                  tRECREM 534 "recrem"
%token                  tWIDTH 537 "width"
%token                  tPERIOD 538 "period"
%token                  tSKEW 535 "skew"
%token                  tBIDIRSKEW 536 "bidirectskew"
%token                  tNOCHANGE 539 "nochange"
%token                  tPORT 205 "port"
%token                  tINTERCONNECT 526 "interconnect"
%token                  tNETDELAY 527 "netdelay"
%token                  tDEVICE 528 "device"
%token                  tCOND 540 "cond"
%token                  tCONDELSE 312 "condelse"

%token                  tCEQ  600 "==="
%token                  tCNEQ 601 "!=="
%token                  tDEQ  602 "=="
%token                  tDNEQ 603 "!="

%token  <str>           tID 200 "identifier"
%token  <str>           tSTRING 229 "string"
%token  <str>           tINT 228 "integer"
%token  <str>           tREAL 282 "real"


%define parse.error verbose
%expect 0

%%

delay_file:                {
                              ctx->root = sdf_new(S_DELAY_FILE);
                              ctx->hchar = '.';
                              ctx->curr_cell = NULL;
                              ctx->curr_delay = NULL;
                              ctx->curr_valtype = 0;
                              ctx->header_mask = 0;
                           }
                           '(' tDELAYFILE sdf_header sdf_cell_list ')'
                           {
                              YYACCEPT;
                           }
         |                 tEOF
                           {
                              ctx->root = NULL;
                           }
         ;

sdf_header:                sdf_version
         |                 sdf_header sdf_header_qstring
         |                 sdf_header voltage
         |                 sdf_header temperature
         |                 sdf_header hierarchy_divider
         |                 sdf_header time_scale
         ;

sdf_version:               '(' tSDFVERSION tSTRING ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, S_HEADER_SDF_VESION);
                              sdf_set_ident(s, ident_new($3));

                              for (char *start = $3 ;; start++) {
                                 if (strlen(start) < 3) {
                                    error_at(&yyloc, "Invalid SDF version: %s. "
                                             "SDF version shall contain one of: "
                                             "\"1.0\", \"2.0\", \"2.1\", \"3.0\" or \"4.0\".",
                                             $3);
                                    break;
                                 }

                                 if (!strncmp(start, "1.0", 3)){
                                    ctx->std = SDF_STD_1_0;
                                    break;
                                 }
                                 else if (!strncmp(start, "2.0", 3)) {
                                    ctx->std = SDF_STD_2_0;
                                    break;
                                 }
                                 else if (!strncmp(start, "2.1", 3)) {
                                    ctx->std = SDF_STD_2_1;
                                    break;
                                 }
                                 else if (!strncmp(start, "3.0", 3)) {
                                    ctx->std = SDF_STD_3_0;
                                    break;
                                 }
                                 else if (!strncmp(start, "4.0", 3)) {
                                    ctx->std = SDF_STD_4_0;
                                    break;
                                 }
                              }
                              sdf_set_subkind(ctx->root, ctx->std);
                              sdf_add_decl(ctx->root, s);
                           }
         ;

sdf_header_qstring:        '(' qstring_header_entry tSTRING ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, $2);
                              sdf_set_ident(s, ident_new($3));
                              sdf_add_decl(ctx->root, s);
                              check_header_item($2);
                           }
         |                 '(' qstring_header_entry ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, $2);
                              sdf_add_decl(ctx->root, s);
                              check_header_item($2);
                           }
         ;

qstring_header_entry:      tDATE       { $$ = S_HEADER_DATE; }
         |                 tPROCESS    { $$ = S_HEADER_PROCESS; }
         |                 tDESIGN     { $$ = S_HEADER_DESIGN; }
         |                 tVENDOR     { $$ = S_HEADER_VENDOR; }
         |                 tPROGRAM    { $$ = S_HEADER_PROGRAM; }
         |                 tVERSION    { $$ = S_HEADER_VERSION; }
         ;

voltage:                   '(' tVOLTAGE signed_real_or_rtripple ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, S_HEADER_VOLTAGE);
                              sdf_set_number(s, $3);
                              sdf_add_decl(ctx->root, s);
                              check_header_item(S_HEADER_VOLTAGE);
                           }
         ;

temperature:               '(' tTEMPERATURE signed_real_or_rtripple ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, S_HEADER_TEMPERATURE);
                              sdf_set_number(s, $3);
                              sdf_add_decl(ctx->root, s);
                              check_header_item(S_HEADER_TEMPERATURE);
                           }
         ;

hierarchy_divider:         '(' tDIVIDER hchar ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, S_HEADER_DIVIDER);
                              char div[2];
                              sprintf(div, "%c", $3);
                              sdf_set_ident(s, ident_new(div));
                              sdf_add_decl(ctx->root, s);
                              check_header_item(S_HEADER_DIVIDER);

                              ctx->hchar = $3;

                           }
         ;

// Do not define tDOT and tOVER as YACC tokens, they are alreday defined
hchar:                     '.'
                           {
                              $$ = tDOT;
                           }
         |                 '/'
                           {
                              $$ = tOVER;
                           }
         ;

time_scale:                '(' tTIMESCALE timescale_number timescale_unit ')'
                           {
                              sdf_node_t s = sdf_new(S_HEADER_ITEM);
                              sdf_set_subkind(s, S_HEADER_TIMESCALE);
                              sdf_set_number(s, $3);
                              sdf_set_ident(s, $4);

                              sdf_add_decl(ctx->root, s);
                           }
         ;

timescale_number:          real_number
                           {
                              double num = sdf_dval($1);

                              // This accepts also timescale_number like 1.000000
                              if (!doubles_equal(num, 1.0)   &&
                                  !doubles_equal(num, 10.0)  &&
                                  !doubles_equal(num, 100.0))
                                  error_at(&yyloc, "Invalid timescale_number: %.1f. "
                                          "Allowed values are: 1, 10, 100, 1.0, 10.0, 100.0", num);
                           }

timescale_unit:            tID
                           {
                              if (strcmp($1, "s")  &&
                                  strcmp($1, "ms") &&
                                  strcmp($1, "us") &&
                                  strcmp($1, "ns") &&
                                  strcmp($1, "ps") &&
                                  strcmp($1, "fs"))
                                 error_at(&yyloc, "Invalid timescale_unit: %s. "
                                          "Allowed values are: s, ms, us, ps, fs.", $1);

                              $$ = ident_new($1);
                           }

sdf_cell_list:             sdf_cell_list sdf_cell
                           {
                              insert_sdf_cell($2);
                           }
         |                 sdf_cell
                           {
                              insert_sdf_cell($1);
                           }
         ;

sdf_cell:                  sdf_cell_head ')'
         |                 sdf_cell_head timing_spec ')'
         ;

sdf_cell_head:             '(' tCELL
                           {
                              ctx->curr_cell = sdf_new(S_CELL);
                           }
                           celltype cell_instance
                           {
                              $$ = ctx->curr_cell;
                              sdf_set_ident($$, $4);
                              sdf_set_ident2($$, $5);
                           }
         ;

celltype:                  '(' tCELLTYPE tSTRING ')'
                           {
                              $$ = ident_new($3);
                              free($3);
                           }
         ;

cell_instance:             '(' tINSTANCE hierarchical_identifier ')'
                           {
                              $$ = $3;
                           }
         |                 '(' tINSTANCE '*' ')'
                           {
                              $$ = ident_new("*");
                           }
         ;

hierarchical_identifier:   identifier
         |                 identifier hchar hierarchical_identifier
                           {
                              // Convert to use always dot to keep internal representation
                              // the same as rest of NVC
                              $$ = ident_prefix($1, $3, '.');

                              if ($2 != ctx->hchar)
                                 warn_at(&yyloc, "Hierarchy separator: %c different than "
                                          "hierarchy separator in SDF header: %c", $2, ctx->hchar);

                           }
         ;

identifier:                tID
                           {
                              $$ = ident_new($1);
                              free($1);
                           }
         ;

// TODO: Implement te_spec
timing_spec:               del_spec
         |                 tc_spec
         |                 lbl_spec
         ;

lbl_spec:                  '(' tLABEL lbl_type_list ')'
         ;

lbl_type_list:             lbl_type_list lbl_type
         |                 lbl_type
         ;

lbl_type:                  '(' lbl_type_kind lbl_def_list ')'
         ;

lbl_type_kind:             tABSOLUTE
                           {
                              ctx->curr_valtype = S_F_VALUE_ABSOLUTE;
                           }
         |                 tINCREMENT
                           {
                              ctx->curr_valtype = S_F_VALUE_INCREMENT;
                           }
         ;

lbl_def_list:              lbl_def_list lbl_def
         |                 lbl_def
         ;

lbl_def:                   '(' identifier
                           {
                              sdf_node_t lbl = sdf_new(S_LABEL);
                              sdf_set_ident(lbl, $2);
                              sdf_set_flag(lbl, ctx->curr_valtype);
                              sdf_add_label(ctx->curr_cell, lbl);
                              // Reuse current delay for current label. Delvals are
                              // both "value" for these
                              ctx->curr_delay = lbl;
                           }
                           delval_list ')'

         ;

tc_spec:                   '(' tTIMINGCHECK tchk_def_list ')'
         ;

tchk_def_list:             tchk_def_list tchk_def
                           {
                              sdf_add_tcheck(ctx->curr_cell, $2);
                           }
         |                 tchk_def
                           {
                              sdf_add_tcheck(ctx->curr_cell, $1);
                           }
         ;

tchk_def:                  tchk_2_port_1_val
         |                 tchk_merged
         |                 tchk_1_port_1_val
         |                 skew_timing_check
         |                 bidirectskew_timing_check
         |                 nochange_timing_check
         ;

tchk_2_port_1_val:         '(' tchk_2_port_1_val_kind port_tchk port_tchk value ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, $2);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              sdf_add_value($$, $5);
                           }
         ;

tchk_2_port_1_val_kind:    tSETUP
                           {
                              $$ = S_TCHECK_SETUP;
                           }
         |                 tHOLD
                           {
                              $$ = S_TCHECK_HOLD;
                           }
         |                 tRECOVERY
                           {
                              $$ = S_TCHECK_RECOVERY;
                           }
         |                 tREMOVAL
                           {
                              $$ = S_TCHECK_REMOVAL;
                           }
         ;

// TODO: Add optional scond
tchk_merged:               '(' tchk_merged_kind port_tchk port_tchk rvalue rvalue ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, $2);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              sdf_add_value($$, $5);
                              sdf_add_value($$, $6);
                           }
         ;

tchk_merged_kind:          tSETUPHOLD
                           {
                              $$ = S_TCHECK_SETUPHOLD;
                           }
         |                 tRECREM
                           {
                              $$ = S_TCHECK_RECREM;
                           }
         ;

tchk_1_port_1_val:         '(' tchk_1_port_1_val_kind port_tchk value ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, $2);
                              sdf_add_port($$, $3);
                              sdf_add_value($$, $4);
                           }
         ;

tchk_1_port_1_val_kind:    tWIDTH
                           {
                              $$ = S_TCHECK_WIDTH;
                           }
         |                 tPERIOD
                           {
                              $$ = S_TCHECK_PERIOD;
                           }
         ;

skew_timing_check:         '(' tSKEW port_tchk port_tchk rvalue ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, S_TCHECK_SKEW);
                              sdf_add_port($$, $3);
                              sdf_add_value($$, $4);
                           }
         ;

bidirectskew_timing_check: '(' tBIDIRSKEW port_tchk port_tchk value value ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, S_TCHECK_BIDIRSKEW);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              sdf_add_value($$, $5);
                              sdf_add_value($$, $6);
                           }
         ;

nochange_timing_check:     '(' tNOCHANGE port_tchk port_tchk rvalue rvalue ')'
                           {
                              $$ = sdf_new(S_TIMING_CHECK);
                              sdf_set_subkind($$, S_TCHECK_NOCHANGE);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              sdf_add_value($$, $5);
                              sdf_add_value($$, $6);
                           }
         ;

del_spec:                  '(' tDELAY deltype_list ')'
         ;

deltype_list:              deltype_list deltype
         |                 deltype
         ;

deltype:                   absolute_deltype
         |                 increment_deltype
         |                 pathpulse_deltype
         |                 pathpulsepercent_deltype
         ;

absolute_deltype:          '(' tABSOLUTE
                           {
                              ctx->curr_valtype = S_F_VALUE_ABSOLUTE;
                           }
                           del_def_list ')'
         ;

increment_deltype:         '(' tINCREMENT
                           {
                              ctx->curr_valtype = S_F_VALUE_INCREMENT;
                           }
                           del_def_list ')'
         ;

pathpulse_deltype:         '(' tPATHPULSE
                           {
                              ctx->curr_delay = sdf_new(S_DELAY);
                              sdf_set_subkind(ctx->curr_delay, S_DELAY_KIND_PATHPULSE);
                           }
                           input_output_path pp_values ')'
                           {
                              sdf_add_delay(ctx->curr_cell, ctx->curr_delay);
                           }
         ;

pathpulsepercent_deltype:  '(' tPATHPULSEP
                           {
                              ctx->curr_delay = sdf_new(S_DELAY);
                              sdf_set_subkind(ctx->curr_delay, S_DELAY_KIND_PATHPULSEP);
                           }
                           input_output_path pp_values ')'
                           {
                              sdf_add_delay(ctx->curr_cell, ctx->curr_delay);
                           }
         ;

input_output_path:         port_instance port_instance
                           {
                              sdf_add_port(ctx->curr_delay, $1);
                              sdf_add_port(ctx->curr_delay, $2);
                           }
         |
         ;

pp_values:                 value
                           {
                              sdf_add_value(ctx->curr_delay, $1);
                           }
         |                 value value
                           {
                              sdf_add_value(ctx->curr_delay, $1);
                              sdf_add_value(ctx->curr_delay, $2);
                           }
         ;

del_def_list:              del_def_list
                           {
                              ctx->curr_delay = sdf_new(S_DELAY);
                           }
                           del_def
                           {
                              sdf_add_delay(ctx->curr_cell, $3);
                           }

         |                 {
                              ctx->curr_delay = sdf_new(S_DELAY);
                           }
                           del_def
                           {
                              sdf_add_delay(ctx->curr_cell, $2);
                           }
         ;

del_def:                   iopath_def
         |                 cond_def
         |                 condelse_def
         |                 port_def
         |                 interconnect_def
         |                 netdelay_def
         |                 device_def
         ;

// TODO: Add retain_def for IOPATHs
iopath_def:                '(' tIOPATH port_spec port_instance delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_IOPATH);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              check_delval_list_len($$);
                           }
         ;

// TODO: Add optional qstring
cond_def:                  '(' tCOND conditional_port_expr iopath_def ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_node_t c = sdf_new(S_COND);
                              sdf_set_expr(c, $3);
                              sdf_add_cond($$, c);
                           }
         ;

// TODO: Implement expression parsing.
conditional_port_expr:     port
         ;

condelse_def:              '(' tCONDELSE iopath_def ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_node_t c = sdf_new(S_COND);
                              sdf_add_cond($$, c);
                           }
         ;

port_def:                  '(' tPORT port_instance delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_PORT);
                              sdf_add_port($$, $3);
                              check_delval_list_len($$);
                           }
         ;

interconnect_def:          '(' tINTERCONNECT port_instance port_instance delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_INTERCONNECT);
                              sdf_add_port($$, $3);
                              sdf_add_port($$, $4);
                              check_delval_list_len($$);
                           }
         ;

// Grammar for port_spec derives the same as for net_spec
// TODO: Should we somehow track difference between "net" and "port"
//       (flag, separate sdf_kind_t) ? Should we then differentiate/check if a delay
//       for "net" is applied to something of type "port" and vice-versa?
netdelay_def:              '(' tNETDELAY port_spec delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_NETDELAY);
                              sdf_add_port($$, $3);
                              check_delval_list_len($$);
                           }
         ;

device_def:                '(' tDEVICE port_spec delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_DEVICE);
                              sdf_add_port($$, $3);
                              check_delval_list_len($$);
                           }

         |                 '(' tDEVICE delval_list ')'
                           {
                              $$ = ctx->curr_delay;
                              sdf_set_flag($$, ctx->curr_valtype);
                              sdf_set_subkind($$, S_DELAY_KIND_DEVICE);
                              check_delval_list_len($$);
                           }
         ;

// TODO: Add optional "qstring" COND port_spec
port_tchk:                 port_spec
         |                 '(' tCOND timing_check_condition port_spec ')'
         ;

timing_check_condition:    scalar_port
         |                 inversion_operator scalar_port
         |                 scalar_port equality_operator scalar_constant
         ;

inversion_operator:        '~'
         |                 '!'
         ;

equality_operator:         tDEQ
         |                 tDNEQ
         |                 tCEQ
         |                 tCNEQ
         ;

// TODO: Add other constant formats, need to figure out how to scan them
scalar_constant:           '1'
         |                 '0'
         ;


port_spec:                 port_instance
         |                 port_edge
         ;

port_edge:                 '(' edge_identifier port_instance ')'
                           {
                              $$ = $3;
                              sdf_set_flag($$, $2);
                           }
         ;

// TODO: Implement other edge identifier types!
edge_identifier:           tPOSEDGE
                           {
                              $$ = S_F_POSEDGE;
                           }
         |                 tNEGEDGE
                           {
                              $$ = S_F_NEGEDGE;
                           }
         ;

// TODO: Re-check "hierarchical_identifier hchar port" is not needed here.
port_instance:             port
         ;

port:                      scalar_port
                           {
                              sdf_set_subkind($$, S_PORT_SCALAR);
                           }
         |                 bus_port
                           {
                              sdf_set_subkind($$, S_PORT_BUS);
                           }
         ;

scalar_port:               port_name
         |                 port_name '[' integer ']'
                           {
                              sdf_node_t index = sdf_new(S_NUMBER);
                              sdf_set_ival(index, $3);
                              sdf_set_subkind(index, S_NUMBER_INTEGER);
                              sdf_add_dim($$, index);
                           }
         ;

bus_port:                  port_name '[' integer ':' integer ']'
                           {
                              sdf_node_t left = sdf_new(S_NUMBER);
                              sdf_node_t right = sdf_new(S_NUMBER);
                              sdf_set_subkind(left, S_NUMBER_INTEGER);
                              sdf_set_subkind(right, S_NUMBER_INTEGER);
                              sdf_set_ival(left, $3);
                              sdf_set_ival(right, $5);
                              sdf_add_dim($$, left);
                              sdf_add_dim($$, right);
                           }
         ;

port_name:                 hierarchical_identifier
                           {
                              $$ = sdf_new(S_PORT);
                              sdf_set_ident($$, $1);
                           }
         ;

delval_list:               delval_list delval
                           {
                              sdf_add_value(ctx->curr_delay, $2);
                           }
         |                 delval
                           {
                              sdf_add_value(ctx->curr_delay, $1);
                           }
         ;

delval:                    rvalue
                           {
                              $$ = sdf_new(S_DELVAL);
                              sdf_add_value($$, $1);
                           }
         |                 '(' rvalue rvalue ')'
                           {
                              $$ = sdf_new(S_DELVAL);
                              sdf_add_value($$, $2);
                              sdf_add_value($$, $3);
                           }
         |                 '(' rvalue rvalue rvalue ')'
                           {
                              $$ = sdf_new(S_DELVAL);
                              sdf_add_value($$, $2);
                              sdf_add_value($$, $3);
                              sdf_add_value($$, $4);
                           }
         ;

rvalue:                    '(' signed_real_or_rtripple ')'
                           {
                              $$ = sdf_new(S_VALUE);
                              sdf_set_number($$, $2);
                           }
         |                 '(' ')'
                           {
                              $$ = sdf_new(S_VALUE);
                           }
         ;

signed_real_or_rtripple:   signed_real_number
         |                 rtripple
         ;

rtripple:                  signed_real_number rtripple_tail_2
                           {
                              $$ = $2;
                              sdf_set_min($$, $1);
                           }
         |                 rtripple_tail_2
         ;

rtripple_tail_2:           ':' signed_real_number rtripple_tail_1
                           {
                              $$ = $3;
                              sdf_set_typ($$, $2);
                           }
         |                 ':' rtripple_tail_1
                           {
                              $$ = $2;
                           }
         ;

rtripple_tail_1:           ':' signed_real_number
                           {
                              $$ = sdf_new(S_TRIPPLE);
                              sdf_set_max($$, $2);
                           }
         |                 ':'
                           {
                              $$ = sdf_new(S_TRIPPLE);
                           }
         ;

value:                     '(' real_or_tripple ')'
                           {
                              $$ = sdf_new(S_VALUE);
                              sdf_set_number($$, $2);
                           }
         |                 '(' ')'
                           {
                              $$ = sdf_new(S_VALUE);
                           }
         ;

real_or_tripple:           real_number
         |                 tripple
         ;

tripple:                   real_number tripple_tail_2
                           {
                              $$ = $2;
                              sdf_set_min($$, $1);
                           }
         |                 tripple_tail_2
         ;

tripple_tail_2:            ':' real_number tripple_tail_1
                           {
                              $$ = $3;
                              sdf_set_typ($$, $2);
                           }
         |                 ':' tripple_tail_1
                           {
                              $$ = $2;
                           }
         ;

tripple_tail_1:            ':' real_number
                           {
                              $$ = sdf_new(S_TRIPPLE);
                              sdf_set_max($$, $2);
                           }
         |                 ':'
                           {
                              $$ = sdf_new(S_TRIPPLE);
                           }
         ;

signed_real_number:        '-' real
                           {
                              // Duplicate creating S_NUMBER rather than using real_number
                              // non-terminal and negating dval of its return value!
                              $$ = sdf_new(S_NUMBER);
                              sdf_set_subkind($$, S_NUMBER_DOUBLE);
                              sdf_set_dval($$, -1 * $2);
                           }
         |                 '+' real_number
                           {
                              $$ = $2;
                           }
         |                 real_number
         ;

real_number:               real
                           {
                              $$ = sdf_new(S_NUMBER);
                              sdf_set_subkind($$, S_NUMBER_DOUBLE);
                              sdf_set_dval($$, $1);
                           }

// For now reuses common parse_decimal_literal
real:                      tREAL
                           {
                              $$ = yylval.real;
                           }
         |                 integer
                           {
                              double tmp = (double)$1;
                              $$ = tmp;
                           }
         ;

integer:                   tINT
                           {
                              $$ = yylval.i64;
                           }
         ;


%%

static void yyerror(const char *s)
{
   error_at(&yylloc, "%s", s);
}


sdf_file_t* sdf_parse(void)
{
   make_new_arena();

   ctx = xcalloc(sizeof(struct _sdf_file));
   ctx->hmap = shash_new(4096);
   ctx->nmap = shash_new(1024);

   scan_as_sdf();

   if (yyparse())
      return NULL;
   else
      return ctx;
}

void sdf_free(sdf_file_t *sdf)
{
   // TODO: Walk the hash maps and free both hmap and cmap!
}

void reset_sdf_parser(void)
{
}
