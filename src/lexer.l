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

%%

{COMMENT}       { begin_token(yytext); }

{ENTITY}        { begin_token(yytext); return tENTITY; }
{IS}            { begin_token(yytext); return tIS; }
{END}           { begin_token(yytext); return tEND; }
{GENERIC}       { begin_token(yytext); return tGENERIC; }
{PORT}          { begin_token(yytext); return tPORT; }
{CONSTANT}      { begin_token(yytext); return tCONSTANT; }
{COMPONENT}     { begin_token(yytext); return tCOMPONENT; }
{CONFIGURATION} { begin_token(yytext); return tCONFIGURATION; }
{ARCHITECTURE}  { begin_token(yytext); return tARCHITECTURE; }
{OF}            { begin_token(yytext); return tOF; }
{BEGIN}         { begin_token(yytext); return tBEGIN; }
{AND}           { begin_token(yytext); return tAND; }
{OR}            { begin_token(yytext); return tOR; }
{XOR}           { begin_token(yytext); return tXOR; }
{XNOR}          { begin_token(yytext); return tXNOR; }
{NAND}          { begin_token(yytext); return tNAND; }
{NOR}           { begin_token(yytext); return tNOR; }
{ABS}           { begin_token(yytext); return tABS; }
{NOT}           { begin_token(yytext); return tNOT; }
{ALL}           { begin_token(yytext); return tALL; }
{IN}            { begin_token(yytext); return tIN; }
{OUT}           { begin_token(yytext); return tOUT; }
{BUFFER}        { begin_token(yytext); return tBUFFER; }
{BUS}           { begin_token(yytext); return tBUS; }
{UNAFFECTED}    { begin_token(yytext); return tUNAFFECTED; }
{SIGNAL}        { begin_token(yytext); return tSIGNAL; }
{PROCESS}       { begin_token(yytext); return tPROCESS; }
{WAIT}          { begin_token(yytext); return tWAIT; }
{REPORT}        { begin_token(yytext); return tREPORT; }
{INOUT}         { begin_token(yytext); return tINOUT; }
{LINKAGE}       { begin_token(yytext); return tLINKAGE; }
{VARIABLE}      { begin_token(yytext); return tVARIABLE; }
{FOR}           { begin_token(yytext); return tFOR; }
{TYPE}          { begin_token(yytext); return tTYPE; }
{RANGE}         { begin_token(yytext); return tRANGE; }
{TO}            { begin_token(yytext); return tTO; }
{DOWNTO}        { begin_token(yytext); return tDOWNTO; }
{SUBTYPE}       { begin_token(yytext); return tSUBTYPE; }
{UNITS}         { begin_token(yytext); return tUNITS; }
{PACKAGE}       { begin_token(yytext); return tPACKAGE; }
{LIBRARY}       { begin_token(yytext); return tLIBRARY; }
{USE}           { begin_token(yytext); return tUSE; }
{NULL}          { begin_token(yytext); return tNULL; }
{FUNCTION}      { begin_token(yytext); return tFUNCTION; }
{IMPURE}        { begin_token(yytext); return tIMPURE; }
{PURE}          { begin_token(yytext); return tPURE; }
{RETURN}        { begin_token(yytext); return tRETURN; }
{ARRAY}         { begin_token(yytext); return tARRAY; }
{OTHERS}        { begin_token(yytext); return tOTHERS; }
{ASSERT}        { begin_token(yytext); return tASSERT; }
{SEVERITY}      { begin_token(yytext); return tSEVERITY; }
{ON}            { begin_token(yytext); return tON; }
{MAP}           { begin_token(yytext); return tMAP; }

"("        { begin_token(yytext); return tLPAREN; }
")"        { begin_token(yytext); return tRPAREN; }
";"        { begin_token(yytext); return tSEMI; }
":="       { begin_token(yytext); return tASSIGN; }
":"        { begin_token(yytext); return tCOLON; }
"**"       { begin_token(yytext); return tPOWER; }
","        { begin_token(yytext); return tCOMMA; }
"<>"       { begin_token(yytext); return tBOX; }
"<"        { begin_token(yytext); return tLT; }
"<="       { begin_token(yytext); return tLE; }
">"        { begin_token(yytext); return tGT; }
">="       { begin_token(yytext); return tGE; }
"=>"       { begin_token(yytext); return tASSOC; }
"+"        { begin_token(yytext); return tPLUS; }
"-"        { begin_token(yytext); return tMINUS; }
"*"        { begin_token(yytext); return tTIMES; }
"/="       { begin_token(yytext); return tNEQ; }
"="        { begin_token(yytext); return tEQ; }
"/"        { begin_token(yytext); return tOVER; }
"."        { begin_token(yytext); return tDOT; }
{INT}      { begin_token(yytext); lvals.ival = atoll(yytext); return tINT; }
{STRING}   { begin_token(yytext); lvals.sval = strdup(yytext); return tSTRING; }
{CHAR}     { begin_token(yytext); lvals.sval = strdup(yytext); return tID; }
{TICK}     { begin_token(yytext); return tTICK; }
{ID}       { begin_token(yytext); return parse_id(yytext); }
{EXID}     { begin_token(yytext); return parse_ex_id(yytext); }
{SPACE}    { begin_token(yytext); }
<<EOF>>    { begin_token(yytext); return 0; }
.          { begin_token(yytext); return tERROR; }
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
