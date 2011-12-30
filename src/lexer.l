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

static int parse_id(const char *str);
static int parse_ex_id(const char *str);

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

%%

{COMMENT}       { }

{ENTITY}        { return tENTITY; }
{IS}            { return tIS; }
{END}           { return tEND; }
{GENERIC}       { return tGENERIC; }
{PORT}          { return tPORT; }
{CONSTANT}      { return tCONSTANT; }
{COMPONENT}     { return tCOMPONENT; }
{CONFIGURATION} { return tCONFIGURATION; }
{ARCHITECTURE}  { return tARCHITECTURE; }
{OF}            { return tOF; }
{BEGIN}         { return tBEGIN; }
{AND}           { return tAND; }
{OR}            { return tOR; }
{XOR}           { return tXOR; }
{XNOR}          { return tXNOR; }
{NAND}          { return tNAND; }
{NOR}           { return tNOR; }
{ABS}           { return tABS; }
{NOT}           { return tNOT; }
{ALL}           { return tALL; }
{IN}            { return tIN; }
{OUT}           { return tOUT; }
{BUFFER}        { return tBUFFER; }
{BUS}           { return tBUS; }
{UNAFFECTED}    { return tUNAFFECTED; }
{SIGNAL}        { return tSIGNAL; }
{PROCESS}       { return tPROCESS; }
{WAIT}          { return tWAIT; }
{REPORT}        { return tREPORT; }
{INOUT}         { return tINOUT; }
{LINKAGE}       { return tLINKAGE; }
{VARIABLE}      { return tVARIABLE; }
{FOR}           { return tFOR; }
{TYPE}          { return tTYPE; }
{RANGE}         { return tRANGE; }
{TO}            { return tTO; }
{DOWNTO}        { return tDOWNTO; }
{SUBTYPE}       { return tSUBTYPE; }
{UNITS}         { return tUNITS; }
{PACKAGE}       { return tPACKAGE; }
{LIBRARY}       { return tLIBRARY; }
{USE}           { return tUSE; }
{NULL}          { return tNULL; }
{FUNCTION}      { return tFUNCTION; }
{IMPURE}        { return tIMPURE; }
{PURE}          { return tPURE; }
{RETURN}        { return tRETURN; }
{ARRAY}         { return tARRAY; }
{OTHERS}        { return tOTHERS; }
{ASSERT}        { return tASSERT; }
{SEVERITY}      { return tSEVERITY; }
{ON}            { return tON; }
{MAP}           { return tMAP; }
{IF}            { return tIF; }
{THEN}          { return tTHEN; }
{ELSE}          { return tELSE; }
{ELSIF}         { return tELSIF; }
{BODY}          { return tBODY; }
{WHILE}         { return tWHILE; }
{LOOP}          { return tLOOP; }
{AFTER}         { return tAFTER; }
{ALIAS}         { return tALIAS; }
{MOD}           { return tMOD; }
{ATTRIBUTE}     { return tATTRIBUTE; }
{PROCEDURE}     { return tPROCEDURE; }
{EXIT}          { return tEXIT; }
{REM}           { return tREM; }
{WHEN}          { return tWHEN; }
{CASE}          { return tCASE; }

"("        { return tLPAREN; }
")"        { return tRPAREN; }
";"        { return tSEMI; }
":="       { return tASSIGN; }
":"        { return tCOLON; }
"**"       { return tPOWER; }
","        { return tCOMMA; }
"<>"       { return tBOX; }
"<"        { return tLT; }
"<="       { return tLE; }
">"        { return tGT; }
">="       { return tGE; }
"=>"       { return tASSOC; }
"+"        { return tPLUS; }
"-"        { return tMINUS; }
"*"        { return tTIMES; }
"/="       { return tNEQ; }
"="        { return tEQ; }
"/"        { return tOVER; }
"."        { return tDOT; }
"&"        { return tAMP; }
"|"        { return tBAR; }
{INT}      { lvals.ival = atoll(yytext); return tINT; }
{STRING}   { lvals.sval = strdup(yytext); return tSTRING; }
{CHAR}     { lvals.sval = strdup(yytext); return tID; }
{TICK}     { return tTICK; }
{ID}       { return parse_id(yytext); }
{EXID}     { return parse_ex_id(yytext); }
{SPACE}    { }
<<EOF>>    { return 0; }
.          { return tERROR; }
%%

static int parse_id(const char *str)
{
   char *p = (lvals.sval = xmalloc(strlen(str) + 1));
   while (*str)
      *p++ = toupper(*str++);
   *p = '\0';

   return tID;
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

   return tID;
}
