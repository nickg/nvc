/* -*- mode: c -*- */

/*
 *  Copyright (C) 2011  Nick Gasson
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */


%option noyywrap
%option nounput
%option noinput

%{
#include "parse.h"
#include "util.h"

#include <ctype.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION begin_token(yytext);

#define TOKEN(t) return (last_token = t);

static int parse_id(const char *str);
static int parse_ex_id(const char *str);
static int resolve_ir1045(void);

static int last_token = -1;
lvals_t lvals;
%}

/* TODO: look at string_literal and character_literal */

ID      [a-zA-Z][a-zA-Z_0-9]*
EXID    \\.*\\
STRING  \".*\"
CHAR    '.'
COMMENT --.*\n
INT     [0-9]+
SPACE   [ \t\r\n]+
TICK    \'

A       [aA]
B       [bB]
C       [cC]
D       [dD]
E       [eE]
F       [fF]
G       [gG]
H       [hH]
I       [iI]
J       [jJ]
K       [kK]
L       [lL]
N       [nN]
M       [mM]
O       [oO]
P       [pP]
R       [rR]
S       [sS]
T       [tT]
U       [uU]
V       [vV]
W       [wW]
Y       [yY]
X       [xX]

ENTITY        {E}{N}{T}{I}{T}{Y}
IS            {I}{S}
END           {E}{N}{D}
GENERIC       {G}{E}{N}{E}{R}{I}{C}
PORT          {P}{O}{R}{T}
CONSTANT      {C}{O}{N}{S}{T}{A}{N}{T}
CONFIGURATION {C}{O}{N}{F}{I}{G}{U}{R}{A}{T}{I}{O}{N}
COMPONENT     {C}{O}{M}{P}{O}{N}{E}{N}{T}
ARCHITECTURE  {A}{R}{C}{H}{I}{T}{E}{C}{T}{U}{R}{E}
OF            {O}{F}
BEGIN         {B}{E}{G}{I}{N}
AND           {A}{N}{D}
OR            {O}{R}
XOR           {X}{O}{R}
XNOR          {X}{N}{O}{R}
NOR           {N}{O}{R}
NAND          {N}{A}{N}{D}
ABS           {A}{B}{S}
NOT           {N}{O}{T}
ALL           {A}{L}{L}
IN            {I}{N}
OUT           {O}{U}{T}
BUFFER        {B}{U}{F}{F}{E}{R}
BUS           {B}{U}{S}
UNAFFECTED    {U}{N}{A}{F}{F}{E}{C}{T}{E}{D}
SIGNAL        {S}{I}{G}{N}{A}{L}
PROCESS       {P}{R}{O}{C}{E}{S}{S}
WAIT          {W}{A}{I}{T}
REPORT        {R}{E}{P}{O}{R}{T}
INOUT         {I}{N}{O}{U}{T}
LINKAGE       {L}{I}{N}{K}{A}{G}{E}
VARIABLE      {V}{A}{R}{I}{A}{B}{L}{E}
FOR           {F}{O}{R}
TYPE          {T}{Y}{P}{E}
RANGE         {R}{A}{N}{G}{E}
TO            {T}{O}
DOWNTO        {D}{O}{W}{N}{T}{O}
SUBTYPE       {S}{U}{B}{T}{Y}{P}{E}
UNITS         {U}{N}{I}{T}{S}
PACKAGE       {P}{A}{C}{K}{A}{G}{E}
LIBRARY       {L}{I}{B}{R}{A}{R}{Y}
USE           {U}{S}{E}
NULL          {N}{U}{L}{L}
FUNCTION      {F}{U}{N}{C}{T}{I}{O}{N}
IMPURE        {I}{M}{P}{U}{R}{E}
PURE          {P}{U}{R}{E}
RETURN        {R}{E}{T}{U}{R}{N}
ARRAY         {A}{R}{R}{A}{Y}
OTHERS        {O}{T}{H}{E}{R}{S}
ASSERT        {A}{S}{S}{E}{R}{T}
SEVERITY      {S}{E}{V}{E}{R}{I}{T}{Y}
ON            {O}{N}
MAP           {M}{A}{P}
IF            {I}{F}
THEN          {T}{H}{E}{N}
ELSE          {E}{L}{S}{E}
ELSIF         {E}{L}{S}{I}{F}
BODY          {B}{O}{D}{Y}
WHILE         {W}{H}{I}{L}{E}
LOOP          {L}{O}{O}{P}
AFTER         {A}{F}{T}{E}{R}
ALIAS         {A}{L}{I}{A}{S}
MOD           {M}{O}{D}
ATTRIBUTE     {A}{T}{T}{R}{I}{B}{U}{T}{E}
PROCEDURE     {P}{R}{O}{C}{E}{D}{U}{R}{E}
EXIT          {E}{X}{I}{T}
REM           {R}{E}{M}
WHEN          {W}{H}{E}{N}
CASE          {C}{A}{S}{E}
TRANSPORT     {T}{R}{A}{N}{S}{P}{O}{R}{T}
INERTIAL      {I}{N}{E}{R}{T}{I}{A}{L}
REJECT        {R}{E}{J}{E}{C}{T}

%%

{COMMENT}       { }

{ENTITY}        { TOKEN(tENTITY); }
{IS}            { TOKEN(tIS); }
{END}           { TOKEN(tEND); }
{GENERIC}       { TOKEN(tGENERIC); }
{PORT}          { TOKEN(tPORT); }
{CONSTANT}      { TOKEN(tCONSTANT); }
{COMPONENT}     { TOKEN(tCOMPONENT); }
{CONFIGURATION} { TOKEN(tCONFIGURATION); }
{ARCHITECTURE}  { TOKEN(tARCHITECTURE); }
{OF}            { TOKEN(tOF); }
{BEGIN}         { TOKEN(tBEGIN); }
{AND}           { TOKEN(tAND); }
{OR}            { TOKEN(tOR); }
{XOR}           { TOKEN(tXOR); }
{XNOR}          { TOKEN(tXNOR); }
{NAND}          { TOKEN(tNAND); }
{NOR}           { TOKEN(tNOR); }
{ABS}           { TOKEN(tABS); }
{NOT}           { TOKEN(tNOT); }
{ALL}           { TOKEN(tALL); }
{IN}            { TOKEN(tIN); }
{OUT}           { TOKEN(tOUT); }
{BUFFER}        { TOKEN(tBUFFER); }
{BUS}           { TOKEN(tBUS); }
{UNAFFECTED}    { TOKEN(tUNAFFECTED); }
{SIGNAL}        { TOKEN(tSIGNAL); }
{PROCESS}       { TOKEN(tPROCESS); }
{WAIT}          { TOKEN(tWAIT); }
{REPORT}        { TOKEN(tREPORT); }
{INOUT}         { TOKEN(tINOUT); }
{LINKAGE}       { TOKEN(tLINKAGE); }
{VARIABLE}      { TOKEN(tVARIABLE); }
{FOR}           { TOKEN(tFOR); }
{TYPE}          { TOKEN(tTYPE); }
{RANGE}         { TOKEN(tRANGE); }
{TO}            { TOKEN(tTO); }
{DOWNTO}        { TOKEN(tDOWNTO); }
{SUBTYPE}       { TOKEN(tSUBTYPE); }
{UNITS}         { TOKEN(tUNITS); }
{PACKAGE}       { TOKEN(tPACKAGE); }
{LIBRARY}       { TOKEN(tLIBRARY); }
{USE}           { TOKEN(tUSE); }
{NULL}          { TOKEN(tNULL); }
{FUNCTION}      { TOKEN(tFUNCTION); }
{IMPURE}        { TOKEN(tIMPURE); }
{PURE}          { TOKEN(tPURE); }
{RETURN}        { TOKEN(tRETURN); }
{ARRAY}         { TOKEN(tARRAY); }
{OTHERS}        { TOKEN(tOTHERS); }
{ASSERT}        { TOKEN(tASSERT); }
{SEVERITY}      { TOKEN(tSEVERITY); }
{ON}            { TOKEN(tON); }
{MAP}           { TOKEN(tMAP); }
{IF}            { TOKEN(tIF); }
{THEN}          { TOKEN(tTHEN); }
{ELSE}          { TOKEN(tELSE); }
{ELSIF}         { TOKEN(tELSIF); }
{BODY}          { TOKEN(tBODY); }
{WHILE}         { TOKEN(tWHILE); }
{LOOP}          { TOKEN(tLOOP); }
{AFTER}         { TOKEN(tAFTER); }
{ALIAS}         { TOKEN(tALIAS); }
{MOD}           { TOKEN(tMOD); }
{ATTRIBUTE}     { TOKEN(tATTRIBUTE); }
{PROCEDURE}     { TOKEN(tPROCEDURE); }
{EXIT}          { TOKEN(tEXIT); }
{REM}           { TOKEN(tREM); }
{WHEN}          { TOKEN(tWHEN); }
{CASE}          { TOKEN(tCASE); }
{TRANSPORT}     { TOKEN(tTRANSPORT); }
{REJECT}        { TOKEN(tREJECT); }
{INERTIAL}      { TOKEN(tINERTIAL); }

"("        { TOKEN(tLPAREN); }
")"        { TOKEN(tRPAREN); }
";"        { TOKEN(tSEMI); }
":="       { TOKEN(tASSIGN); }
":"        { TOKEN(tCOLON); }
"**"       { TOKEN(tPOWER); }
","        { TOKEN(tCOMMA); }
"<>"       { TOKEN(tBOX); }
"<"        { TOKEN(tLT); }
"<="       { TOKEN(tLE); }
">"        { TOKEN(tGT); }
">="       { TOKEN(tGE); }
"=>"       { TOKEN(tASSOC); }
"+"        { TOKEN(tPLUS); }
"-"        { TOKEN(tMINUS); }
"*"        { TOKEN(tTIMES); }
"/="       { TOKEN(tNEQ); }
"="        { TOKEN(tEQ); }
"/"        { TOKEN(tOVER); }
"."        { TOKEN(tDOT); }
"&"        { TOKEN(tAMP); }
"|"        { TOKEN(tBAR); }
"["        { TOKEN(tLSQUARE); }
"]"        { TOKEN(tRSQUARE); }
{INT}      { lvals.ival = atoll(yytext); TOKEN(tINT); }
{STRING}   { lvals.sval = strdup(yytext); TOKEN(tSTRING); }
{TICK}     { TOKEN(tTICK); }
{CHAR}     { if (resolve_ir1045()) {
                lvals.sval = strdup(yytext);
                TOKEN(tID);
             }
             REJECT;
           }
{ID}       { return parse_id(yytext); }
{EXID}     { return parse_ex_id(yytext); }
{SPACE}    { }
<<EOF>>    { return 0; }
.          { TOKEN(tERROR); }
%%

static int resolve_ir1045(void)
{
   // See here for discussion:
   //   http://www.eda-stds.org/isac/IRs-VHDL-93/IR1045.txt
   // The set of tokens that may precede a character literal is
   // disjoint from that which may precede a single tick token.

   switch (last_token) {
   case tRSQUARE:
   case tRPAREN:
   case tALL:
   case tID:
      // Cannot be a character literal
      return 0;
   default:
      return 1;
   }
}

static int parse_id(const char *str)
{
   char *p = (lvals.sval = xmalloc(strlen(str) + 1));
   while (*str)
      *p++ = toupper(*str++);
   *p = '\0';

   TOKEN(tID);
}

static int parse_ex_id(const char *str)
{
   char *p = (lvals.sval = xmalloc(strlen(str) + 1));
   while (*str) {
      switch (*str) {
      case '\\':
         if (*(str + 1) == '\\') {
            str++;
            break;
         }
      default:
         *p++ = *str++;
      }
   }
   *p = '\0';

   TOKEN(tID);
}
