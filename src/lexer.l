/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2011-2022  Nick Gasson
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
#include "util.h"
#include "common.h"
#include "diag.h"
#include "scan.h"
#include "tree.h"

#include <ctype.h>
#include <math.h>
#include <string.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION begin_token(yytext, yyleng);

#define TOKEN(t) return (last_token = (t))

#define TOKEN_LRM(t, lrm) do {                                          \
      static bool warned = false;                                       \
      if (!warned && standard() < lrm) {                                \
         warn_lrm(lrm, "`%s' is a reserved word in VHDL-%s",            \
                  yytext, standard_text(lrm));                          \
         warned = true;                                                 \
         return isalnum(yytext[0]) ? parse_id(yytext) : tERROR;         \
      }                                                                 \
      else                                                              \
         return (last_token = (t));                                     \
   } while (0)

#define MIN_STD(lrm, what) do {                                         \
      static bool warned = false;                                       \
      if (!warned && standard() < lrm) {                                \
         warn_lrm(lrm, "%s are a VHDL-%s feature", what,                \
                  standard_text(lrm));                                  \
         warned = true;                                                 \
      }                                                                 \
   } while (0)

#define TOKEN_00(t) TOKEN_LRM(t, STD_00)
#define TOKEN_08(t) TOKEN_LRM(t, STD_08)

static int parse_id(const char *str);
static int parse_ex_id(const char *str);
static int parse_bit_string(const char *str);
static int parse_string(const char *str);
static int parse_decimal_literal(const char *str);
static int parse_based_literal(const char *str);
static int resolve_ir1045(void);
static void warn_lrm(vhdl_standard_t std, const char *fmt, ...);

static int last_token = -1;

extern loc_t yylloc;

yylval_t yylval;
%}

ID              ?i:[a-z][a-z_0-9]*
EXID            \\([^\\]|\\\\)*\\
STRING          (\"([^\"]|\"\")*\")|(\%([^\"\%]|\%\%)*\%)
BIT_STRING      [0-9]*[usUS]?(?i:[boxd]\"[^\"]+\")|(?i:[boxd]\%[^\%]+\%)
CHAR            '.'
COMMENT         --.*
INTEGER         [0-9][0-9_]*
EXPONENT        [Ee][+-]?{INTEGER}
DECIMAL_LITERAL {INTEGER}(\.{INTEGER})?{EXPONENT}?
BASED_INTEGER   [0-9a-fA-F][0-9a-fA-F_]*
BASED_LITERAL   ({INTEGER}[#]{BASED_INTEGER}(\.{BASED_INTEGER})?[#]{EXPONENT}?)|({INTEGER}[:]{BASED_INTEGER}(\.{BASED_INTEGER})?[:]{EXPONENT}?)
SPACE           [ \t\r]+
TICK            \'
PRAGMA          --[ \t]*
SYNTH_OFF       {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_off).*
SYNTH_ON        {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_on).*

%x COMMENT C_COMMENT

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
REGISTER      ?i:register
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
DISCONNECT    ?i:disconnect
FORCE         ?i:force
RELEASE       ?i:release

%%

{SYNTH_OFF}          { TOKEN(tSYNTHOFF); }
{SYNTH_ON}           { TOKEN(tSYNTHON); }

{COMMENT}            { BEGIN(COMMENT); }

<COMMENT>\n          { BEGIN(INITIAL); }
<COMMENT>.*          { }

"/*"                 { MIN_STD(STD_08, "delimited comments");
                       BEGIN(C_COMMENT);
                     }
<C_COMMENT>"*/"      { BEGIN(INITIAL); }
<C_COMMENT>\n        { /* Must match a single character */ }
<C_COMMENT>[^*]+     { }

{ENTITY}             { TOKEN(tENTITY); }
{IS}                 { TOKEN(tIS); }
{END}                { TOKEN(tEND); }
{GENERIC}            { TOKEN(tGENERIC); }
{PORT}               { TOKEN(tPORT); }
{CONSTANT}           { TOKEN(tCONSTANT); }
{COMPONENT}          { TOKEN(tCOMPONENT); }
{CONFIGURATION}      { TOKEN(tCONFIGURATION); }
{ARCHITECTURE}       { TOKEN(tARCHITECTURE); }
{OF}                 { TOKEN(tOF); }
{BEGIN}              { TOKEN(tBEGIN); }
{AND}                { TOKEN(tAND); }
{OR}                 { TOKEN(tOR); }
{XOR}                { TOKEN(tXOR); }
{XNOR}               { TOKEN(tXNOR); }
{NAND}               { TOKEN(tNAND); }
{NOR}                { TOKEN(tNOR); }
{ABS}                { TOKEN(tABS); }
{NOT}                { TOKEN(tNOT); }
{ALL}                { TOKEN(tALL); }
{IN}                 { TOKEN(tIN); }
{OUT}                { TOKEN(tOUT); }
{BUFFER}             { TOKEN(tBUFFER); }
{BUS}                { TOKEN(tBUS); }
{REGISTER}           { TOKEN(tREGISTER); }
{UNAFFECTED}         { TOKEN(tUNAFFECTED); }
{SIGNAL}             { TOKEN(tSIGNAL); }
{PROCESS}            { TOKEN(tPROCESS); }
{WAIT}               { TOKEN(tWAIT); }
{REPORT}             { TOKEN(tREPORT); }
{INOUT}              { TOKEN(tINOUT); }
{LINKAGE}            { TOKEN(tLINKAGE); }
{VARIABLE}           { TOKEN(tVARIABLE); }
{FOR}                { TOKEN(tFOR); }
{TYPE}               { TOKEN(tTYPE); }
{RANGE}              { TOKEN(tRANGE); }
{TO}                 { TOKEN(tTO); }
{DOWNTO}             { TOKEN(tDOWNTO); }
{SUBTYPE}            { TOKEN(tSUBTYPE); }
{UNITS}              { TOKEN(tUNITS); }
{PACKAGE}            { TOKEN(tPACKAGE); }
{LIBRARY}            { TOKEN(tLIBRARY); }
{USE}                { TOKEN(tUSE); }
{NULL}               { TOKEN(tNULL); }
{FUNCTION}           { TOKEN(tFUNCTION); }
{IMPURE}             { TOKEN(tIMPURE); }
{PURE}               { TOKEN(tPURE); }
{RETURN}             { TOKEN(tRETURN); }
{ARRAY}              { TOKEN(tARRAY); }
{OTHERS}             { TOKEN(tOTHERS); }
{ASSERT}             { TOKEN(tASSERT); }
{SEVERITY}           { TOKEN(tSEVERITY); }
{ON}                 { TOKEN(tON); }
{MAP}                { TOKEN(tMAP); }
{IF}                 { TOKEN(tIF); }
{THEN}               { TOKEN(tTHEN); }
{ELSE}               { TOKEN(tELSE); }
{ELSIF}              { TOKEN(tELSIF); }
{BODY}               { TOKEN(tBODY); }
{WHILE}              { TOKEN(tWHILE); }
{LOOP}               { TOKEN(tLOOP); }
{AFTER}              { TOKEN(tAFTER); }
{ALIAS}              { TOKEN(tALIAS); }
{MOD}                { TOKEN(tMOD); }
{ATTRIBUTE}          { TOKEN(tATTRIBUTE); }
{PROCEDURE}          { TOKEN(tPROCEDURE); }
{POSTPONED}          { TOKEN(tPOSTPONED); }
{EXIT}               { TOKEN(tEXIT); }
{REM}                { TOKEN(tREM); }
{WHEN}               { TOKEN(tWHEN); }
{CASE}               { TOKEN(tCASE); }
{TRANSPORT}          { TOKEN(tTRANSPORT); }
{REJECT}             { TOKEN(tREJECT); }
{INERTIAL}           { TOKEN(tINERTIAL); }
{BLOCK}              { TOKEN(tBLOCK); }
{WITH}               { TOKEN(tWITH); }
{SELECT}             { TOKEN(tSELECT); }
{GENERATE}           { TOKEN(tGENERATE); }
{ACCESS}             { TOKEN(tACCESS); }
{FILE}               { TOKEN(tFILE); }
{OPEN}               { TOKEN(tOPEN); }
{UNTIL}              { TOKEN(tUNTIL); }
{RECORD}             { TOKEN(tRECORD); }
{NEW}                { TOKEN(tNEW); }
{SHARED}             { TOKEN(tSHARED); }
{NEXT}               { TOKEN(tNEXT); }
{SLL}                { TOKEN(tSLL); }
{SRL}                { TOKEN(tSRL); }
{SLA}                { TOKEN(tSLA); }
{SRA}                { TOKEN(tSRA); }
{ROL}                { TOKEN(tROL); }
{ROR}                { TOKEN(tROR); }
{LITERAL}            { TOKEN(tLITERAL); }
{GROUP}              { TOKEN(tGROUP); }
{LABEL}              { TOKEN(tLABEL); }
{GUARDED}            { TOKEN(tGUARDED); }
{DISCONNECT}         { TOKEN(tDISCONNECT); }
{REVRANGE}           { TOKEN(tREVRANGE); }
{PROTECTED}          { TOKEN_00(tPROTECTED); }
{CONTEXT}            { TOKEN_08(tCONTEXT); }
{FORCE}              { TOKEN_08(tFORCE); }
{RELEASE}            { TOKEN_08(tRELEASE); }
`{IF}                { TOKEN(tCONDIF); }
`{ELSE}              { TOKEN(tCONDELSE); }
`{ELSIF}             { TOKEN(tCONDELSIF); }
`{END}               { TOKEN(tCONDEND); }
`{ERROR}             { TOKEN(tCONDERROR); }
`{WARNING}           { TOKEN(tCONDWARN); }

"("                  { TOKEN(tLPAREN); }
")"                  { TOKEN(tRPAREN); }
";"                  { TOKEN(tSEMI); }
":="                 { TOKEN(tASSIGN); }
":"                  { TOKEN(tCOLON); }
"**"                 { TOKEN(tPOWER); }
","                  { TOKEN(tCOMMA); }
"<>"                 { TOKEN(tBOX); }
"<"                  { TOKEN(tLT); }
"<="                 { TOKEN(tLE); }
">"                  { TOKEN(tGT); }
">="                 { TOKEN(tGE); }
"?<"                 { TOKEN_08(tMLT); }
"?<="                { TOKEN_08(tMLE); }
"?>"                 { TOKEN_08(tMGT); }
"?>="                { TOKEN_08(tMGE); }
"??"                 { TOKEN_08(tCCONV); }
"=>"                 { TOKEN(tASSOC); }
"+"                  { TOKEN(tPLUS); }
"-"                  { TOKEN(tMINUS); }
"*"                  { TOKEN(tTIMES); }
"/="                 { TOKEN(tNEQ); }
"="                  { TOKEN(tEQ); }
"?/="                { TOKEN_08(tMNEQ); }
"?="                 { TOKEN_08(tMEQ); }
"/"                  { TOKEN(tOVER); }
"."                  { TOKEN(tDOT); }
"&"                  { TOKEN(tAMP); }
"|"                  { TOKEN(tBAR); }
"!"                  { TOKEN(tBAR); }
"["                  { TOKEN(tLSQUARE); }
"]"                  { TOKEN(tRSQUARE); }
"<<"                 { TOKEN(tLTLT); }
">>"                 { TOKEN(tGTGT); }
"^"                  { TOKEN(tCARET); }
"@"                  { TOKEN(tAT); }
{DECIMAL_LITERAL}    { return parse_decimal_literal(yytext); }
{BASED_LITERAL}      { return parse_based_literal(yytext); }
{BIT_STRING}         { return parse_bit_string(yytext); }
{STRING}             { return parse_string(yytext); }
{TICK}               { TOKEN(tTICK); }
{CHAR}               { if (resolve_ir1045()) {
                          yylval.s = xstrdup(yytext);
                          TOKEN(tID);
                       }
                       REJECT;
                     }
{ID}                 { return parse_id(yytext); }
{EXID}               { return parse_ex_id(yytext); }
{SPACE}              { }
"\n"                 { /* Must match a single character */ }
<*><<EOF>>           { return 0; }
.                    { TOKEN(tERROR); }
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

   // Replacing double '\\' character by single '\\'
   *p++ = *str++;
   while (*str) {
      if ((*str == '\\') && (*(str+1) == '\\')) str++;
      *p++ = *str++;
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

static int parse_decimal_literal(const char *str)
{
   // Transform a string into a literal as specified in LRM 13.4.1
   //   decimal_literal ::= integer [.integer] [exponent]

   int tok = tERROR;
   char *tmp LOCAL = xstrdup(str);
   strip_underscores(tmp);

   char *dot = strpbrk(tmp, ".");

   if (dot == NULL) {
      char *sign = strpbrk(tmp, "-");
      char *val  = strtok(tmp, "eE");
      char *exp  = strtok(NULL, "eE");

      yylval.n = atoll(val);

      long long int e = (exp ? atoll(exp) : 0);

      if (e >= 0) {  // Minus sign forbidden for an integer literal
         for (; e > 0; e--) yylval.n *= 10;
         tok = (sign == NULL) ? tINT : tERROR;
      }
   }
   else {
      yylval.d = strtod(tmp, NULL);
      tok = tREAL;
   }

   TOKEN(tok);
}

/**
 * \fn static int parse_based_literal (const char *str)
 * \brief
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
   // Transform a string into a literal as specified in LRM 13.4.2
   //   based_literal ::= base [#:] based_integer [.based_integer] [#:]
   //     [exponent]

   int tok = tERROR;
   char *tmp LOCAL = xstrdup(str);
   strip_underscores(tmp);

   char *dot  = strpbrk(tmp, ".");
   char *sign = strpbrk(tmp, "-");
   char *base = strtok(tmp, "#:");
   char *val  = strtok(NULL, "#:");
   char *exp  = strtok(NULL, "eE");

   // Base must be at least 2 and at most 16
   if ((2 <= atoi(base)) && (atoi(base) <= 16)) {
      if (dot == NULL) {
         char *eptr;
         yylval.n = strtoll(val, &eptr, atoi(base));

         long long int e = (exp ? atoll(exp) : 0);

         if (e >= 0) {  // Minus sign forbidden for an integer literal
            for (; e > 0; e--) yylval.n *= atoi(base);
            tok = ((*eptr == '\0') && (sign == NULL)) ? tINT : tERROR;
         }
      }
      else {
         char *eptr_integer, *eptr_rational;
         char *integer  = strtok(val, ".");
         char *rational = strtok(NULL, ".");

         yylval.d = (double)strtoll(integer, &eptr_integer, atoi(base));

         double tmp = (double)strtoll(rational, &eptr_rational, atoi(base));
         tmp *= pow((double)atoi(base), (double)((long)(0 - strlen(rational))));

         yylval.d += tmp;

         long long int e = (exp ? atoll(exp) : 0);

         if (e != 0)
            yylval.d *= pow((double) atoi(base), (double) e);

         if (*eptr_integer == '\0' && *eptr_rational == '\0')
            tok = tREAL;
         else
            tok = tERROR;
      }
   }

   TOKEN(tok);
}

static int parse_string(const char *str)
{
   // Replaces all double '\"' by single '\"' or all double '%' by
   // single '%'.  In the case when '\%' is used as string brackets, the
   // enclosed senquence of characters should not contain quotation
   // marks!

   char *s = (yylval.s = xstrdup(str));

   // Replacing double '\"' character by single '\"' or double '%'
   // character by single '%'
   // Begins after first character
   s++;
   char *p = s;
   while (*p) {
      if ((*p == *(yylval.s)) && (*(p+1) == *(yylval.s))) p++;
      *s++ = *p++;
   }
   *s = *p;

   TOKEN(tSTRING);
}

static int parse_bit_string(const char *str)
{
   // Copy input, remove all '_' characters and replace all '\%'
   // characters by '\"'.

   char *p = (yylval.s = xstrdup(str));

   strip_underscores(p);

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

static void warn_lrm(vhdl_standard_t std, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   diag_t *d = diag_new(DIAG_WARN, &yylloc);
   diag_vprintf(d, fmt, ap);
   diag_hint(d, NULL, "pass $bold$--std=%s$$ to enable this feature",
             standard_text(std));
   diag_emit(d);

   va_end(ap);
}

void reset_vhdl_scanner(void)
{
   YY_FLUSH_BUFFER;
}
