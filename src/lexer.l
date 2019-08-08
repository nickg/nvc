/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2011-2019  Nick Gasson
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
#include "common.h"

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

#define TOKEN_LRM(t, lrm)                                       \
   if (standard() < lrm) {                                      \
      warn_at(&yylloc, "%s is a reserved word in VHDL-%s",      \
              yytext, standard_text(lrm));                      \
      return parse_id(yytext);                                  \
   }                                                            \
   else                                                         \
      return (last_token = (t));

#define TOKEN_00(t) TOKEN_LRM(t, STD_00)
#define TOKEN_08(t) TOKEN_LRM(t, STD_08)

static int parse_id(const char *str);
static int parse_ex_id(const char *str);
static int parse_bit_string(const char *str);
static int parse_string(const char *str);
static int parse_decimal_literal(const char *str);
static int parse_based_literal(const char *str);
static int resolve_ir1045(void);

static int last_token = -1;

extern loc_t yylloc;

yylval_t yylval;

void begin_token(char *tok);
int get_next_char(char *b, int max_buffer);
%}

ID              ?i:[a-z][a-z_0-9]*
EXID            \\([^\\]|\\\\)*\\
STRING          (\"([^\"]|\"\")*\")|(\%([^\"\%]|\%\%)*\%)
BIT_STRING      (?i:[box]\"([0-9a-f][0-9a-f_]*)?\")|(?i:[box]\%([0-9a-f][0-9a-f_]*)?\%)
CHAR            '.'
COMMENT         --.*
INTEGER         [0-9][0-9_]*
EXPONENT        [Ee][+-]?{INTEGER}
DECIMAL_LITERAL {INTEGER}(\.{INTEGER})?{EXPONENT}?
BASED_INTEGER   [0-9a-fA-F][0-9a-fA-F_]*
BASED_LITERAL   ({INTEGER}[#]{BASED_INTEGER}(\.{BASED_INTEGER})?[#]{EXPONENT}?)|({INTEGER}[:]{BASED_INTEGER}(\.{BASED_INTEGER})?[:]{EXPONENT}?)
SPACE           [ \t\r\n]+
TICK            \'
PRAGMA          --[ \t]*
/*
SYNTH_OFF       {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_off).*
SYNTH_ON        {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_on).*
*/

ENTITY        ?i:entity
IS            ?i:is
END           ?i:end
GENERIC       ?i:generic
PORT          ?i:port
CONSTANT      ?i:constant
CONFIGURATION ?i:configuration
COMPONENT     ?i:component
ARCHITECTURE  ?i:architecture
OF            ?i:of
BEGIN         ?i:begin
AND           ?i:and
OR            ?i:or
XOR           ?i:xor
XNOR          ?i:xnor
NOR           ?i:nor
NAND          ?i:nand
ABS           ?i:abs
NOT           ?i:not
ALL           ?i:all
IN            ?i:in
OUT           ?i:out
BUFFER        ?i:buffer
BUS           ?i:bus
UNAFFECTED    ?i:unaffected
SIGNAL        ?i:signal
PROCESS       ?i:process
WAIT          ?i:wait
REPORT        ?i:report
INOUT         ?i:inout
LINKAGE       ?i:linkage
VARIABLE      ?i:variable
FOR           ?i:for
TYPE          ?i:type
RANGE         ?i:range
TO            ?i:to
DOWNTO        ?i:downto
SUBTYPE       ?i:subtype
UNITS         ?i:units
PACKAGE       ?i:package
LIBRARY       ?i:library
USE           ?i:use
NULL          ?i:null
FUNCTION      ?i:function
IMPURE        ?i:impure
PURE          ?i:pure
RETURN        ?i:return
ARRAY         ?i:array
OTHERS        ?i:others
ASSERT        ?i:assert
SEVERITY      ?i:severity
ON            ?i:on
MAP           ?i:map
IF            ?i:if
THEN          ?i:then
ELSE          ?i:else
ELSIF         ?i:elsif
BODY          ?i:body
WHILE         ?i:while
LOOP          ?i:loop
AFTER         ?i:after
ALIAS         ?i:alias
MOD           ?i:mod
ATTRIBUTE     ?i:attribute
PROCEDURE     ?i:procedure
EXIT          ?i:exit
REM           ?i:rem
WHEN          ?i:when
CASE          ?i:case
TRANSPORT     ?i:transport
INERTIAL      ?i:inertial
REJECT        ?i:reject
BLOCK         ?i:block
WITH          ?i:with
SELECT        ?i:select
GENERATE      ?i:generate
ACCESS        ?i:access
POSTPONED     ?i:postponed
FILE          ?i:file
OPEN          ?i:open
UNTIL         ?i:until
RECORD        ?i:record
NEW           ?i:new
SHARED        ?i:shared
NEXT          ?i:next
SLL           ?i:sll
SRL           ?i:srl
SLA           ?i:sla
SRA           ?i:sra
ROL           ?i:rol
ROR           ?i:ror
GROUP         ?i:group
LABEL         ?i:label
LITERAL       ?i:literal
GUARDED       ?i:guarded
REVRANGE      ?i:reverse_range
PROTECTED     ?i:protected
CONTEXT       ?i:context
ERROR         ?i:error
WARNING       ?i:warning

/* Pragma triggers */
COVERAGE_ON   ?:coverage_on
COVERAGE_OFF  ?:coverage_off
LINT_ON       ?:lint_on
LINT_OFF      ?:lint_off
TRACING_ON    ?:tracing_on
TRACING_OFF   ?:tracing_off
SYNTHESIS     ?:synthesis
SYNOPSYS      ?:synopsys

/*
{SYNTH_OFF}     { TOKEN(tSYNTHOFF); }
{SYNTH_ON}      { TOKEN(tSYNTHON); }
*/

%%

{PRAGMA}{COVERAGE_ON}.*  { TOKEN(tPRAGMA); }
{PRAGMA}{COVERAGE_OFF}.* { TOKEN(tPRAGMA); }
{PRAGMA}{LINT_ON}.*      { TOKEN(tPRAGMA); }
{PRAGMA}{LINT_OFF}.*     { TOKEN(tPRAGMA); }
{PRAGMA}{TRACING_ON}.*   { TOKEN(tPRAGMA); }
{PRAGMA}{TRACING_OFF}.*  { TOKEN(tPRAGMA); }
{PRAGMA}{SYNTHESIS}.*    { TOKEN(tPRAGMA); }
{PRAGMA}{SYNOPSYS}.*     { TOKEN(tPRAGMA); }

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
{PROTECTED}     { TOKEN_00(tPROTECTED); }
{CONTEXT}       { TOKEN_08(tCONTEXT); }
`{IF}           { TOKEN(tCONDIF); }
`{ELSE}         { TOKEN(tCONDELSE); }
`{ELSIF}        { TOKEN(tCONDELSIF); }
`{END}          { TOKEN(tCONDEND); }
`{ERROR}        { TOKEN(tCONDERROR); }
`{WARNING}      { TOKEN(tCONDWARN); }

"("               { TOKEN(tLPAREN); }
")"               { TOKEN(tRPAREN); }
";"               { TOKEN(tSEMI); }
":="              { TOKEN(tASSIGN); }
":"               { TOKEN(tCOLON); }
"**"              { TOKEN(tPOWER); }
","               { TOKEN(tCOMMA); }
"<>"              { TOKEN(tBOX); }
"<"               { TOKEN(tLT); }
"<="              { TOKEN(tLE); }
">"               { TOKEN(tGT); }
">="              { TOKEN(tGE); }
"=>"              { TOKEN(tASSOC); }
"+"               { TOKEN(tPLUS); }
"-"               { TOKEN(tMINUS); }
"*"               { TOKEN(tTIMES); }
"/="              { TOKEN(tNEQ); }
"="               { TOKEN(tEQ); }
"/"               { TOKEN(tOVER); }
"."               { TOKEN(tDOT); }
"&"               { TOKEN(tAMP); }
"|"               { TOKEN(tBAR); }
"!"               { TOKEN(tBAR); }
"["               { TOKEN(tLSQUARE); }
"]"               { TOKEN(tRSQUARE); }
{DECIMAL_LITERAL} { return parse_decimal_literal(yytext); }
{BASED_LITERAL}   { return parse_based_literal(yytext); }
{BIT_STRING}      { return parse_bit_string(yytext); }
{STRING}          { return parse_string(yytext); }
{TICK}            { TOKEN(tTICK); }
{CHAR}            { if (resolve_ir1045()) {
                       yylval.s = xstrdup(yytext);
                       TOKEN(tID);
                    }
                    REJECT;
                  }
{ID}              { return parse_id(yytext); }
{EXID}            { return parse_ex_id(yytext); }
{SPACE}           { }
<<EOF>>           { return 0; }
.                 { TOKEN(tERROR); }
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
   /* Copy of input */
   char *p = (yylval.s = xmalloc(strlen(str) + 1));

   /* Replacing double '\\' character by single '\\' */
   /* Begins after first character */
   *p++ = *str++;
   while (*str) {
      if ((*str == '\\') && (*(str+1) == '\\')) str++;
      *p++ = *str++;
   }
   /* Copy last character which is null character */
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

/**
 * \fn static int parse_decimal_literal (const char *str)
 * \brief Transform a string into a literal as specified
 * in LRM 13.4.1 (decimal_literal ::= integer [.integer] [exponent]).
 *
 * \param str Formatted string as specified in
 * LRM 13.4.1 (decimal_literal ::= integer [.integer] [exponent]).
 * \return TOKEN(tINT) if no '.' found and no '-' found,
 * TOKEN(tERROR) if no '.' found and '-' found,
 * else TOKEN(tREAL).
 */
static int parse_decimal_literal (const char *str)
{
   int tok = tERROR;
   /* Copy of input text */
   char *tmp LOCAL = xstrdup(str);
   /* Stripping '_' character */
   strip_underscores(tmp);

   /* Find first occurences of '.' */
   char *dot = strpbrk(tmp, ".");

   /* Integer literals */
   if (dot == NULL) {
      /* Find first occurences of 'e' or 'E' */
      char *sign = strpbrk(tmp, "-");
      char *val  = strtok(tmp, "eE");
      char *exp  = strtok(NULL, "eE");

      /* String to long long */
      yylval.n = atoll(val);

      /* String to long long (atoll function manages the sign) */
      long long int e = (exp ? atoll(exp) : 0);

      /* Minus sign forbidden for an integer literal */
      if (e >= 0) {
         /* Updating val */
         for (; e > 0; e--) yylval.n *= 10;

         tok = (sign == NULL) ? tINT : tERROR;
      }
   }
   else {
      /* String to double */
      yylval.d = strtod(tmp, NULL);
      tok = tREAL;
   }

   TOKEN(tok);
}

/**
 * \fn static int parse_based_literal (const char *str)
 * \brief Transform a string into a literal as specified in LRM 13.4.2
 * (based_literal ::= base [#:] based_integer [.based_integer] [#:] [exponent]).
 *
 * \param str Formatted string as specified in LRM 13.4.2
 * (based_literal ::= base [#:] based_integer [.based_integer] [#:] [exponent]).
 * \return TOKEN(tINT) if no '.' found and no '-' found,
 * TOKEN(tERROR) if no '.' found and '-' found,
 * TOKEN(tERROR) if the base is not at least 2 or at most 16,
 * else TOKEN(tREAL).
 */
static int parse_based_literal (const char *str)
{
   int tok = tERROR;
   /* Copy of input text */
   char *tmp = xstrdup(str);
   /* Stripping '_' character */
   strip_underscores(tmp);

   /* Find first occurences of '.' */
   char *dot  = strpbrk(tmp, ".");
   char *sign = strpbrk(tmp, "-");
   /* Find first occurences of '#', ':', 'e' or 'E' */
   char *base = strtok(tmp, "#:");
   char *val  = strtok(NULL, "#:");
   char *exp  = strtok(NULL, "eE");

   /* base must be at least 2 and at most 16 */
   if ((2 <= atoi(base)) && (atoi(base) <= 16)) {
      /* Integer literals */
      if (dot == NULL) {
         char *eptr;
         /* String to long long */
         yylval.n = strtoll(val, &eptr, atoi(base));

         /* String to long long (atoll function manages the sign) */
         long long int e = (exp ? atoll(exp) : 0);

         /* Minus sign forbidden for an integer literal */
         if (e >= 0) {
            /* Updating val */
            for (; e > 0; e--) yylval.n *= atoi(base);

            tok = ((*eptr == '\0') && (sign == NULL)) ? tINT : tERROR;
         }
      }
      else {
         char *eptr_integer, *eptr_rational;
         char *integer  = strtok(val, ".");
         char *rational = strtok(NULL, ".");

         yylval.d = (double) strtoll(integer, &eptr_integer, atoi(base));

         double tmp = (double) strtoll(rational, &eptr_rational, atoi(base));
         tmp *= pow((double) atoi(base), (double) ((long int) (0-strlen(rational))));

         yylval.d += tmp;

         /* String to long long (atoll function manages the sign) */
         long long int e = (exp ? atoll(exp) : 0);

         if (e != 0)
            yylval.d *= pow((double) atoi(base), (double) e);

         tok = ((*eptr_integer == '\0') && (*eptr_rational == '\0')) ? tREAL : tERROR;
      }
   }
   /* Deleting local allocation */
   free(tmp);

   TOKEN(tok);
}

/**
 * \fn static int parse_string (const char *str)
 * \brief Copy input replaces all double '\"' by single '\"'
 *        or all double '%' by single '%'.
 *        In the case when '\%' is used as string brackets,
 *        the enclosed senquence of characters should not
 *        contain quotation marks!
 *
 * \param str Formatted string as specified in LRM 13.6
 * (string_literal ::= " { graphic_character } ").
 * \return TOKEN(tSTRING).
 */
static int parse_string(const char *str)
{
   /* Copy of input */
   char *s = (yylval.s = xstrdup(str));

   /* Replacing double '\"' character by single '\"' */
   /* or double '%' character by single '%' */
   /* Begins after first character */
   s++;
   char *p = s;
   while (*p) {
      if ((*p == *(yylval.s)) && (*(p+1) == *(yylval.s))) p++;
      *s++ = *p++;
   }
   /* Copy last character which is null character */
   *s = *p;

   TOKEN(tSTRING);
}

/**
 * \fn static int parse_bit_string (const char *str)
 * \brief Copy input, remove all '_' characters and
 *        replace all '\%' characters by '\"'.
 *
 * \param str Formatted string as specified in LRM 13.7
 * (bit_string_literal ::= base_specifier " [ bit_value ] ").
 * \return TOKEN(tBITSTRING).
 */
static int parse_bit_string(const char *str)
{
   /* Copy of input */
   char *p = (yylval.s = xstrdup(str));

   /* Stripping '_' character */
   strip_underscores(p);

   /* Replacing '\%' character by '\"' */
   while (*p) {
      switch (*p) {
      case '%':
         *p = '\"';
      default:
         p++;
      }
   }

   TOKEN(tBITSTRING);
}
