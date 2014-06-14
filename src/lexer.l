/* -*- mode: c -*- */

/*
 *  Copyright (C) 2011-2014  Nick Gasson
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
#include "tree.h"
#include "util.h"
#include "token.h"

#include <ctype.h>
#include <math.h>
#include <string.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION begin_token(yytext);

#define TOKEN(t) return (last_token = (t))

static int parse_id(const char *str);
static int parse_ex_id(const char *str);
static int parse_based_int(const char *str);
static int parse_real(const char *str);
static int parse_int(const char *str);
static int resolve_ir1045(void);

static int last_token = -1;

yylval_t yylval;

void begin_token(char *tok);
int get_next_char(char *b, int max_buffer);
%}

ID         [a-zA-Z][a-zA-Z_0-9]*
EXID       \\.*\\
STRING     \"([^\"]|\"\")*\"
BIT_STRING [BOXbox]\"[0-9a-fA-f_]+\"
CHAR       '.'
COMMENT    --.*
DIGITS     [0-9][0-9_]*
INT        {DIGITS}{EXPONENT}?
REAL       {INT}\.{INT}
EXPONENT   [Ee][+-]?{DIGITS}
BASED_INT  {INT}[#:][0-9a-fA-f][0-9a-fA-f_]*[#:]
SPACE      [ \t\r\n]+
TICK       \'

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
BLOCK         {B}{L}{O}{C}{K}
WITH          {W}{I}{T}{H}
SELECT        {S}{E}{L}{E}{C}{T}
GENERATE      {G}{E}{N}{E}{R}{A}{T}{E}
ACCESS        {A}{C}{C}{E}{S}{S}
POSTPONED     {P}{O}{S}{T}{P}{O}{N}{E}{D}
FILE          {F}{I}{L}{E}
OPEN          {O}{P}{E}{N}
UNTIL         {U}{N}{T}{I}{L}
RECORD        {R}{E}{C}{O}{R}{D}
NEW           {N}{E}{W}
SHARED        {S}{H}{A}{R}{E}{D}
NEXT          {N}{E}{X}{T}
SLL           {S}{L}{L}
SRL           {S}{R}{L}
SLA           {S}{L}{A}
SRA           {S}{R}{A}
ROL           {R}{O}{L}
ROR           {R}{O}{R}
GROUP         {G}{R}{O}{U}{P}
LABEL         {L}{A}{B}{E}{L}
LITERAL       {L}{I}{T}{E}{R}{A}{L}
GUARDED       {G}{U}{A}{R}{D}{E}{D}
REVRANGE      {R}{E}{V}{E}{R}{S}{E}_{R}{A}{N}{G}{E}

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
{POSTPONED}     { TOKEN(tPOSTPONED); }
{EXIT}          { TOKEN(tEXIT); }
{REM}           { TOKEN(tREM); }
{WHEN}          { TOKEN(tWHEN); }
{CASE}          { TOKEN(tCASE); }
{TRANSPORT}     { TOKEN(tTRANSPORT); }
{REJECT}        { TOKEN(tREJECT); }
{INERTIAL}      { TOKEN(tINERTIAL); }
{BLOCK}         { TOKEN(tBLOCK); }
{WITH}          { TOKEN(tWITH); }
{SELECT}        { TOKEN(tSELECT); }
{GENERATE}      { TOKEN(tGENERATE); }
{ACCESS}        { TOKEN(tACCESS); }
{FILE}          { TOKEN(tFILE); }
{OPEN}          { TOKEN(tOPEN); }
{UNTIL}         { TOKEN(tUNTIL); }
{RECORD}        { TOKEN(tRECORD); }
{NEW}           { TOKEN(tNEW); }
{SHARED}        { TOKEN(tSHARED); }
{NEXT}          { TOKEN(tNEXT); }
{SLL}           { TOKEN(tSLL); }
{SRL}           { TOKEN(tSRL); }
{SLA}           { TOKEN(tSLA); }
{SRA}           { TOKEN(tSRA); }
{ROL}           { TOKEN(tROL); }
{ROR}           { TOKEN(tROR); }
{LITERAL}       { TOKEN(tLITERAL); }
{GROUP}         { TOKEN(tGROUP); }
{LABEL}         { TOKEN(tLABEL); }
{GUARDED}       { TOKEN(tGUARDED); }
{REVRANGE}      { TOKEN(tREVRANGE); }

"("          { TOKEN(tLPAREN); }
")"          { TOKEN(tRPAREN); }
";"          { TOKEN(tSEMI); }
":="         { TOKEN(tASSIGN); }
":"          { TOKEN(tCOLON); }
"**"         { TOKEN(tPOWER); }
","          { TOKEN(tCOMMA); }
"<>"         { TOKEN(tBOX); }
"<"          { TOKEN(tLT); }
"<="         { TOKEN(tLE); }
">"          { TOKEN(tGT); }
">="         { TOKEN(tGE); }
"=>"         { TOKEN(tASSOC); }
"+"          { TOKEN(tPLUS); }
"-"          { TOKEN(tMINUS); }
"*"          { TOKEN(tTIMES); }
"/="         { TOKEN(tNEQ); }
"="          { TOKEN(tEQ); }
"/"          { TOKEN(tOVER); }
"."          { TOKEN(tDOT); }
"&"          { TOKEN(tAMP); }
"|"          { TOKEN(tBAR); }
"!"          { TOKEN(tBAR); }
"["          { TOKEN(tLSQUARE); }
"]"          { TOKEN(tRSQUARE); }
{INT}        { return parse_int(yytext); }
{BASED_INT}  { return parse_based_int(yytext); }
{REAL}       { return parse_real(yytext); }
{BIT_STRING} { yylval.s = strdup(yytext); TOKEN(tBITSTRING); }
{STRING}     { yylval.s = strdup(yytext); TOKEN(tSTRING); }
{TICK}       { TOKEN(tTICK); }
{CHAR}       { if (resolve_ir1045()) {
                  yylval.s = strdup(yytext);
                  TOKEN(tID);
               }
               REJECT;
             }
{ID}         { return parse_id(yytext); }
{EXID}       { return parse_ex_id(yytext); }
{SPACE}      { }
<<EOF>>      { return 0; }
.            { TOKEN(tERROR); }
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
   char *p = (yylval.s = xmalloc(strlen(str) + 1));
   while (*str) {
      *p++ = toupper((int)*str);
      str++;
   }
   *p = '\0';

   TOKEN(tID);
}

static int parse_ex_id(const char *str)
{
   char *p = (yylval.s = xmalloc(strlen(str) + 1));
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

static void strip_underscores(char *s)
{
   char *p;
   for (p = s; *s != '\0'; s++)
      if (*s != '_')
         *p++ = *s;
   *p = '\0';
}

static int parse_int(const char *str)
{
   char *tmp = strdup(str);
   strip_underscores(tmp);

   char *base = strtok(tmp, "eE");
   char *exp  = strtok(NULL, "eE");

   yylval.n = atoll(base);

   int e = (exp ? atoi(exp) : 0);

   free(tmp);

   if (e < 0)
      TOKEN(tERROR);
   else {
      for (; e > 0; e--)
         yylval.n *= 10;
   }

   TOKEN(tINT);
}

static int parse_based_int(const char *str)
{
   char *tmp = strdup(str);
   strip_underscores(tmp);

   char *base  = strtok(tmp, "#:");
   char *value = strtok(NULL, "#:");

   char btok = str[value - tmp - 1];
   char etok = str[strlen(str) - 1];

   char *eptr = NULL;
   yylval.n = strtol(value, &eptr, atoi(base));
   int t = ((*eptr == '\0') && (btok == etok)) ? tINT : tERROR;

   free(tmp);

   TOKEN(t);
}

static int parse_real(const char *str)
{
   char *tmp = strdup(str);
   strip_underscores(tmp);

   char *base = strtok(tmp, "eE");
   char *exp  = strtok(NULL, "eE");

   yylval.d = strtod(base, NULL);

   int e = (exp ? atoi(exp) : 0);
   if (e != 0)
      yylval.d *= pow(10.0, (double)e);

   free(tmp);

   TOKEN(tREAL);
}
