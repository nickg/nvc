/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2011-2024  Nick Gasson
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
#include "ident.h"
#include "option.h"
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
      if (standard() < lrm && isalnum_iso88591(yytext[0])) {            \
         static bool warned = false;                                    \
         const token_t token = parse_id(yytext);                        \
         if (token == tID && !warned) {                                 \
            warn_lrm(lrm, "%s is a reserved word in VHDL-%s",           \
                     istr(yylval.ident), standard_text(lrm));           \
            warned = true;                                              \
         }                                                              \
         return token;                                                  \
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
#define TOKEN_19(t) TOKEN_LRM(t, STD_19)

static int parse_id(char *str);
static int parse_ex_id(char *str);
static int parse_bit_string(const char *str);
static int parse_string(const char *str);
static int parse_decimal_literal(const char *str);
static int parse_verilog_real(const char *str);
static int parse_based_literal(const char *str);
static int resolve_ir1045(void);
static void warn_lrm(vhdl_standard_t std, const char *fmt, ...);
static void warn_utf8(const char *str);
static bool begin_psl_comment(void);
static int escape_verilog_string(const char *str);
static int report_unterminated_string(const char *str);
static int report_bad_identifier(char *str);

static int last_token = -1;
static int comment_caller = 0;

extern loc_t yylloc;
extern yylval_t yylval;
%}

LOWER           [a-z\xdf-\xf6\xf8-\xff]
UPPER           [A-Z\xc0-\xd6\xd8-\xde]
VHDL_ID         ({LOWER}|{UPPER})(_?({LOWER}|{UPPER}|[0-9]))*
BAD_ID          ({LOWER}|{UPPER}|_)({LOWER}|{UPPER}|[0-9_])*
EXID            \\([^\\\n]|\\\\)*\\
VLOG_ID         [a-zA-Z_]([a-zA-Z0-9_$])*
/* TODO: Add "escaped_character" handling to SDF ID */
/* TODO: According to spec, identifier could even start with number*/
SDF_ID          ([a-zA-Z$_])([a-zA-Z$_0-9])*
SYSTASK         \$[a-zA-Z_$]([a-zA-Z0-9_$])*
STRING          (\"([^\"\n]|\"\")*\")|(\%([^\"\n\%]|\%\%)*\%)
BADSTRING       (\"([^\"\n]|\"\")*)|(\%([^\"\n\%]|\%\%)*)
VLOG_STRING     "\""[^\n\"]*"\""
BITSTRING       [0-9]*[usUS]?(?i:[boxd]\"[^\"\n]*\")|(?i:[boxd]\%[^\n\%]*\%)
CHAR            '.'
INTEGER         [0-9][0-9_]*
EXPONENT        [Ee][+-]?{INTEGER}
DECIMAL         {INTEGER}(\.{INTEGER})?{EXPONENT}?
HEX             [0-9a-fA-FzZxX][0-9a-fA-FzZxX_]*
BASED_HASH      {INTEGER}[#]{HEX}(\.{HEX})?[#]{EXPONENT}?
BASED_COLON     {INTEGER}[:]{HEX}(\.{HEX})?[:]{EXPONENT}?
BASED           {BASED_HASH}|{BASED_COLON}
SPACE           [ \t\r]+
TICK            \'
PRAGMA          --[ \t]*
SYNTH_OFF       {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_off).*
SYNTH_ON        {PRAGMA}(?i:synthesis)[ \t]+(?i:translate_on).*
COVERAGE_OFF    {PRAGMA}(?i:coverage)[ \t]+(?i:off).*
COVERAGE_ON     {PRAGMA}(?i:coverage)[ \t]+(?i:on).*
TRANSLATE_OFF   {PRAGMA}(?i:pragma)[ \t]+(?i:translate_off).*
TRANSLATE_ON    {PRAGMA}(?i:pragma)[ \t]+(?i:translate_on).*
PSL_COMMENT     {PRAGMA}(?i:psl)[ \t]+
PSL_CONT        ^{SPACE}*({PSL_COMMENT}|"--")
UTF8_MB         [\x80-\xff][\x80-\xbf]{1,3}
VLOG_NUMBER     {INTEGER}?\'[bhd]{HEX}
VLOG_REAL       {INTEGER}\.{INTEGER}
UDP_LEVEL       [01xX?bB]
UDP_EDGE        [rRfFpPnN*]
UDP_INDICATOR   "("{UDP_LEVEL}{UDP_LEVEL}")"
SCALAR_ZERO     ('b0|'B0|1'b0|1'B0)
SCALAR_ONE      ('b1|'B1|1'b1|1'B1)

%x COMMENT C_COMMENT PSL VLOG UDP SDF SDF_EXPR

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
PARAMETER     ?i:parameter
ALWAYS        ?i:always
DEFAULT       ?i:default
CLOCK         ?i:clock
NEVER         ?i:never
EVENTUALLY    ?i:eventually
NEXT_A        ?i:next_a
NEXT_E        ?i:next_e
NEXT_EVENT    ?i:next_event
ASSUME        ?i:assume
ASSUME_G      ?i:assume_guarantee
RESTRICT      ?i:restrict
RESTRICT_G    ?i:restrict_guarantee
STRONG        ?i:strong
FAIRNESS      ?i:fairness
COVER         ?i:cover
PROPERTY      ?i:property
SEQUENCE      ?i:sequence
CONST         ?i:const
MUTABLE       ?i:mutable
HDLTYPE       ?i:hdltype
BOOLEAN       ?i:boolean
BIT           ?i:bit
BITVECTOR     ?i:bitvector
NUMERIC       ?i:numeric
STRING_K      ?i:string
WITHIN        ?i:within
VIEW          ?i:view
PRIVATE       ?i:private
PREV          ?i:prev
STABLE        ?i:stable
ROSE          ?i:rose
FELL          ?i:fell
ENDED         ?i:ended
NONDET        ?i:nondet
NONDET_V      ?i:nondet_vector
UNION         ?i:union
INF           ?i:inf
ENDPOINT      ?i:endpoint

INTERCONNECT      ?i:interconnect
DELAYFILE         ?i:delayfile
SDFVERSION        ?i:sdfversion
DESIGN            ?i:design
DATE              ?i:date
VENDOR            ?i:vendor
PROGRAM           ?i:program
VERSION           ?i:version
DIVIDER           ?i:divider
VOLTAGE           ?i:voltage
TEMPERATURE       ?i:temperature
TIMESCALE         ?i:timescale
CELL              ?i:cell
CELLTYPE          ?i:celltype
INSTANCE          ?i:instance
ABSOLUTE          ?i:absolute
PATHPULSE         ?i:pathpulse
PATHPULSEP        ?i:pathpulsepercent
INCREMENT         ?i:increment
IOPATH            ?i:iopath
DELAY             ?i:delay
SETUP             ?i:setup
HOLD              ?i:hold
SETUPHOLD         ?i:setuphold
RECOVERY          ?i:recovery
REMOVAL           ?i:removal
RECREM            ?i:recrem
WIDTH             ?i:width
PERIOD            ?i:period
SKEW              ?i:skew
BIDIRSKEW         ?i:bidirectskew
NOCHANGE          ?i:nochange
NETDELAY          ?i:netdelay
DEVICE            ?i:device
COND              ?i:cond
CONDELSE          ?i:condelse
PATHCONSTRAINT    ?i:PATHCONSTRAINT
PERIODCONSTRAINT  ?i:PERIODCONSTRAINT
SUM               ?i:SUM
DIFF              ?i:DIFF
SKEWCONSTRAINT    ?i:SKEWCONSTRAINT
ARRIVAL           ?i:ARRIVAL
DEPARTURE         ?i:DEPARTURE
SLACK             ?i:SLACK
WAVEFORM          ?i:WAVEFORM
NAME              ?i:NAME
EXCEPTION         ?i:EXCEPTION
TIMINGCHECK       ?i:TIMINGCHECK
TIMINGENV         ?i:TIMINGENV
RETAIN            ?i:RETAIN
SCOND             ?i:SCOND
CCOND             ?i:CCOND
ABORT             ?i:abort
ASYNC_ABORT       ?i:async_abort
SYNC_ABORT        ?i:sync_abort
BEFORE            ?i:before

%%

{SYNTH_OFF}              { TOKEN(tSYNTHOFF); }
{SYNTH_ON}               { TOKEN(tSYNTHON); }
{COVERAGE_OFF}           { TOKEN(tCOVERAGEOFF); }
{COVERAGE_ON}            { TOKEN(tCOVERAGEON); }
{TRANSLATE_OFF}          { TOKEN(tTRANSLATEOFF); }
{TRANSLATE_ON}           { TOKEN(tTRANSLATEON); }

<PSL,INITIAL>"--"        { comment_caller = YY_START; BEGIN(COMMENT); }
<VLOG,UDP>"//"           { comment_caller = YY_START; BEGIN(COMMENT); }
{PSL_COMMENT}            { if (begin_psl_comment()) {
                              BEGIN(PSL);
                              TOKEN(tSTARTPSL);
                           }
                           else {
                              comment_caller = YY_START;
                              BEGIN(COMMENT);
                           }
                         }

<COMMENT>\n              { BEGIN(comment_caller); }
<COMMENT>.*              { }

"/*"                     { MIN_STD(STD_08, "delimited comments");
                           comment_caller = YY_START;
                           BEGIN(C_COMMENT);
                         }
<VLOG,UDP>"/*"           { comment_caller = YY_START; BEGIN(C_COMMENT); }
<C_COMMENT>"*/"          { BEGIN(comment_caller); }
<C_COMMENT>\n            { /* Must match a single character */ }
<C_COMMENT>.             { }

<PSL>;                   { TOKEN(tSEMI); }
<PSL>{PSL_CONT}          { /* Multi-line PSL comment */ }
<PSL>"--"                { comment_caller = YY_START; BEGIN(COMMENT); }

{ENTITY}                 { TOKEN(tENTITY); }
{IS}                     { TOKEN(tIS); }
{END}                    { TOKEN(tEND); }
{GENERIC}                { TOKEN(tGENERIC); }
{PORT}                   { TOKEN(tPORT); }
{COMPONENT}              { TOKEN(tCOMPONENT); }
{CONFIGURATION}          { TOKEN(tCONFIGURATION); }
{ARCHITECTURE}           { TOKEN(tARCHITECTURE); }
{OF}                     { TOKEN(tOF); }
{BEGIN}                  { TOKEN(tBEGIN); }
{IN}                     { TOKEN(tIN); }
{OUT}                    { TOKEN(tOUT); }
{BUFFER}                 { TOKEN(tBUFFER); }
{BUS}                    { TOKEN(tBUS); }
{REGISTER}               { TOKEN(tREGISTER); }
{UNAFFECTED}             { TOKEN(tUNAFFECTED); }
{PROCESS}                { TOKEN(tPROCESS); }
{WAIT}                   { TOKEN(tWAIT); }
{REPORT}                 { TOKEN(tREPORT); }
{INOUT}                  { TOKEN(tINOUT); }
{LINKAGE}                { TOKEN(tLINKAGE); }
{FOR}                    { TOKEN(tFOR); }
{TYPE}                   { TOKEN(tTYPE); }
{SUBTYPE}                { TOKEN(tSUBTYPE); }
{UNITS}                  { TOKEN(tUNITS); }
{PACKAGE}                { TOKEN(tPACKAGE); }
{LIBRARY}                { TOKEN(tLIBRARY); }
{USE}                    { TOKEN(tUSE); }
{FUNCTION}               { TOKEN(tFUNCTION); }
{IMPURE}                 { TOKEN(tIMPURE); }
{PURE}                   { TOKEN(tPURE); }
{RETURN}                 { TOKEN(tRETURN); }
{ARRAY}                  { TOKEN(tARRAY); }
{OTHERS}                 { TOKEN(tOTHERS); }
{ASSERT}                 { TOKEN(tASSERT); }
{SEVERITY}               { TOKEN(tSEVERITY); }
{ON}                     { TOKEN(tON); }
{MAP}                    { TOKEN(tMAP); }
{IF}                     { TOKEN(tIF); }
{THEN}                   { TOKEN(tTHEN); }
{ELSE}                   { TOKEN(tELSE); }
{ELSIF}                  { TOKEN(tELSIF); }
{BODY}                   { TOKEN(tBODY); }
{WHILE}                  { TOKEN(tWHILE); }
{LOOP}                   { TOKEN(tLOOP); }
{AFTER}                  { TOKEN(tAFTER); }
{ALIAS}                  { TOKEN(tALIAS); }
{ATTRIBUTE}              { TOKEN(tATTRIBUTE); }
{PROCEDURE}              { TOKEN(tPROCEDURE); }
{POSTPONED}              { TOKEN(tPOSTPONED); }
{EXIT}                   { TOKEN(tEXIT); }
{WHEN}                   { TOKEN(tWHEN); }
{CASE}                   { TOKEN(tCASE); }
{TRANSPORT}              { TOKEN(tTRANSPORT); }
{REJECT}                 { TOKEN(tREJECT); }
{INERTIAL}               { TOKEN(tINERTIAL); }
{BLOCK}                  { TOKEN(tBLOCK); }
{WITH}                   { TOKEN(tWITH); }
{SELECT}                 { TOKEN(tSELECT); }
{GENERATE}               { TOKEN(tGENERATE); }
{ACCESS}                 { TOKEN(tACCESS); }
{FILE}                   { TOKEN(tFILE); }
{OPEN}                   { TOKEN(tOPEN); }
{RECORD}                 { TOKEN(tRECORD); }
{NEW}                    { TOKEN(tNEW); }
{SHARED}                 { TOKEN(tSHARED); }
{NEXT}                   { TOKEN(tNEXT); }
{LITERAL}                { TOKEN(tLITERAL); }
{GROUP}                  { TOKEN(tGROUP); }
{LABEL}                  { TOKEN(tLABEL); }
{GUARDED}                { TOKEN(tGUARDED); }
{DISCONNECT}             { TOKEN(tDISCONNECT); }
{REVRANGE}               { TOKEN(tREVRANGE); }
{PROTECTED}              { TOKEN_00(tPROTECTED); }
{CONTEXT}                { TOKEN_08(tCONTEXT); }
{FORCE}                  { TOKEN_08(tFORCE); }
{RELEASE}                { TOKEN_08(tRELEASE); }
{PARAMETER}              { TOKEN_08(tPARAMETER); }
{DEFAULT}                { TOKEN_08(tDEFAULT); }
{COVER}                  { TOKEN_08(tCOVER); }
{VIEW}                   { TOKEN_19(tVIEW); }
{PRIVATE}                { TOKEN_19(tPRIVATE); }

`{IF}                    { TOKEN(tCONDIF); }
`{ELSE}                  { TOKEN(tCONDELSE); }
`{ELSIF}                 { TOKEN(tCONDELSIF); }
`{END}                   { TOKEN(tCONDEND); }
`{ERROR}                 { TOKEN(tCONDERROR); }
`{WARNING}               { TOKEN(tCONDWARN); }

<INITIAL,PSL>{AND}       { TOKEN(tAND); }
<INITIAL,PSL>{OR}        { TOKEN(tOR); }
<INITIAL,PSL>{XOR}       { TOKEN(tXOR); }
<INITIAL,PSL>{XNOR}      { TOKEN(tXNOR); }
<INITIAL,PSL>{NAND}      { TOKEN(tNAND); }
<INITIAL,PSL>{NOR}       { TOKEN(tNOR); }
<INITIAL,PSL>{ABS}       { TOKEN(tABS); }
<INITIAL,PSL>{NOT}       { TOKEN(tNOT); }
<INITIAL,PSL>{ALL}       { TOKEN(tALL); }
<INITIAL,PSL>{TICK}      { TOKEN(tTICK); }
<INITIAL,PSL>{SLL}       { TOKEN(tSLL); }
<INITIAL,PSL>{SRL}       { TOKEN(tSRL); }
<INITIAL,PSL>{SLA}       { TOKEN(tSLA); }
<INITIAL,PSL>{SRA}       { TOKEN(tSRA); }
<INITIAL,PSL>{ROL}       { TOKEN(tROL); }
<INITIAL,PSL>{ROR}       { TOKEN(tROR); }
<INITIAL,PSL>{REM}       { TOKEN(tREM); }
<INITIAL,PSL>{MOD}       { TOKEN(tMOD); }
<INITIAL,PSL>{NULL}      { TOKEN(tNULL); }
<INITIAL,PSL>{RANGE}     { TOKEN(tRANGE); }
<INITIAL,PSL>{TO}        { TOKEN(tTO); }
<INITIAL,PSL>{DOWNTO}    { TOKEN(tDOWNTO); }
<INITIAL,PSL>{UNTIL}     { TOKEN(tUNTIL); }
<INITIAL,PSL>{CONSTANT}  { TOKEN(tCONSTANT); }
<INITIAL,PSL>{SIGNAL}    { TOKEN(tSIGNAL); }
<INITIAL,PSL>{VARIABLE}  { TOKEN(tVARIABLE); }

<UDP>{UDP_LEVEL}         { yylval.i64 = yytext[0]; return tUDPLEVEL; }
<UDP>{UDP_EDGE}          { yylval.i64 = yytext[0]; return tUDPEDGE; }
<UDP>{UDP_INDICATOR}     { yylval.str = xstrdup(yytext); return tUDPIND; }

<*>"("                   { TOKEN(tLPAREN); }
<*>")"                   { TOKEN(tRPAREN); }
<*>"["                   { TOKEN(tLSQUARE); }
<*>"]"                   { TOKEN(tRSQUARE); }
<*>"{"                   { TOKEN(tLBRACE); }
<*>"}"                   { TOKEN(tRBRACE); }
<*>","                   { TOKEN(tCOMMA); }
<*>";"                   { TOKEN(tSEMI); }
<*>":"                   { TOKEN(tCOLON); }
<*>"<"                   { TOKEN(tLT); }
<*>">"                   { TOKEN(tGT); }
<*>"+"                   { TOKEN(tPLUS); }
<*>"-"                   { TOKEN(tMINUS); }
<*>"*"                   { TOKEN(tTIMES); }
<*>"@"                   { TOKEN(tAT); }
<*>"?"                   { TOKEN(tQUESTION); }
<*>"{"                   { TOKEN(tLBRACE); }
<*>"}"                   { TOKEN(tRBRACE); }
<*>"/"                   { TOKEN(tOVER); }
<*>"."                   { TOKEN(tDOT); }
<*>"&"                   { TOKEN(tAMP); }
<*>"|"                   { TOKEN(tBAR); }
<*>"["                   { TOKEN(tLSQUARE); }
<*>"]"                   { TOKEN(tRSQUARE); }
<*>"="                   { TOKEN(tEQ); }
<*>"#"                   { TOKEN(tHASH); }
<*>"~"                   { TOKEN(tTILDE); }
<*>"[["                  { TOKEN(t2LSQUARE); }
<*>"]]"                  { TOKEN(t2RSQUARE); }

<INITIAL,PSL>"!"         { TOKEN(tBAR); }
<VLOG>"!"                { TOKEN(tBANG); }

<INITIAL,PSL>"**"        { TOKEN(tPOWER); }
<INITIAL,PSL>":="        { TOKEN(tWALRUS); }
<INITIAL,PSL>"<>"        { TOKEN(tBOX); }
<INITIAL,PSL>"?<"        { TOKEN_08(tMLT); }
<INITIAL,PSL>"?<="       { TOKEN_08(tMLE); }
<INITIAL,PSL>"?>"        { TOKEN_08(tMGT); }
<INITIAL,PSL>"?>="       { TOKEN_08(tMGE); }
<INITIAL,PSL>"??"        { TOKEN_08(tCCONV); }
<INITIAL,PSL>"/="        { TOKEN(tNEQ); }
<INITIAL,PSL>"?/="       { TOKEN_08(tMNEQ); }
<INITIAL,PSL>"?="        { TOKEN_08(tMEQ); }
<INITIAL,PSL>"<<"        { TOKEN(tLTLT); }
<INITIAL,PSL>">>"        { TOKEN(tGTGT); }
<INITIAL,PSL>"^"         { TOKEN(tCARET); }

<INITIAL,PSL,VLOG>"<="   { TOKEN(tLE); }
<INITIAL,PSL,VLOG>">="   { TOKEN(tGE); }
<INITIAL,PSL,VLOG>"=>"   { TOKEN(tASSOC); }

<PSL>{ASSERT}            { TOKEN(tASSERT); }
<PSL>{ASSUME}            { TOKEN(tASSUME); }
<PSL>{ASSUME_G}          { TOKEN(tASSUMEG); }
<PSL>{RESTRICT}          { TOKEN(tRESTRICT); }
<PSL>{RESTRICT_G}        { TOKEN(tRESTRICTG); }
<PSL>{STRONG}            { TOKEN(tSTRONG); }
<PSL>{FAIRNESS}          { TOKEN(tFAIRNESS); }
<PSL>{COVER}             { TOKEN(tCOVER); }
<PSL>{PROPERTY}          { TOKEN(tPROPERTY); }
<PSL>{SEQUENCE}          { TOKEN(tSEQUENCE); }
<PSL>{CONST}             { TOKEN(tCONST); }
<PSL>{MUTABLE}           { TOKEN(tMUTABLE); }
<PSL>{HDLTYPE}           { TOKEN(tHDLTYPE); }
<PSL>{BOOLEAN}           { TOKEN(tBOOLEAN); }
<PSL>{BIT}               { TOKEN(tBIT); }
<PSL>{BITVECTOR}         { TOKEN(tBITVECTOR); }
<PSL>{NUMERIC}           { TOKEN(tNUMERIC); }
<PSL>{STRING_K}          { TOKEN(tSTRINGK); }
<PSL>{ALWAYS}            { TOKEN(tALWAYS); }
<PSL>{DEFAULT}           { TOKEN(tDEFAULT); }
<PSL>{CLOCK}             { TOKEN(tCLOCK); }
<PSL>{IS}                { TOKEN(tIS); }
<PSL>{IN}                { TOKEN(tIN); }
<PSL>{FOR}               { TOKEN(tFOR); }
<PSL>"->"                { TOKEN(tIFIMPL); }
<PSL>"<->"               { TOKEN(tIFFIMPL); }
<PSL>{NEXT}              { TOKEN(tPSLNEXT); }
<PSL>{NEXT}!             { TOKEN(tNEXT1); }
<PSL>{NEVER}             { TOKEN(tNEVER); }
<PSL>{EVENTUALLY}!       { TOKEN(tEVENTUALLY); }
<PSL>{NEXT_A}            { TOKEN(tNEXTA); }
<PSL>{NEXT_A}!           { TOKEN(tNEXTA1); }
<PSL>{NEXT_E}            { TOKEN(tNEXTE); }
<PSL>{NEXT_E}!           { TOKEN(tNEXTE1); }
<PSL>{NEXT_EVENT}        { TOKEN(tNEXTEVENT); }
<PSL>{NEXT_EVENT}!       { TOKEN(tNEXTEVENT1); }
<PSL>"[*"                { TOKEN(tTIMESRPT); }
<PSL>"[+]"               { TOKEN(tPLUSRPT); }
<PSL>"[="                { TOKEN(tEQRPT); }
<PSL>"[->"               { TOKEN(tGOTORPT); }
<PSL>"&&"                { TOKEN(tDBLAMP); }
<PSL>{WITHIN}            { TOKEN(tWITHIN); }
<PSL>{PREV}              { TOKEN(tPREV); }
<PSL>{STABLE}            { TOKEN(tSTABLE); }
<PSL>{ROSE}              { TOKEN(tROSE); }
<PSL>{FELL}              { TOKEN(tFELL); }
<PSL>{ENDED}             { TOKEN(tENDED); }
<PSL>{NONDET}            { TOKEN(tNONDET); }
<PSL>{NONDET_V}          { TOKEN(tNONDETV); }
<PSL>{UNION}             { TOKEN(tUNION); }
<PSL>{UNTIL}!            { TOKEN(tUNTIL1); }
<PSL>{UNTIL}_            { TOKEN(tUNTIL_); }
<PSL>{UNTIL}_!           { TOKEN(tUNTIL1_); }
<PSL>{ABORT}             { TOKEN(tABORT); }
<PSL>{ASYNC_ABORT}       { TOKEN(tASYNC_ABORT); }
<PSL>{SYNC_ABORT}        { TOKEN(tSYNC_ABORT); }
<PSL>{BEFORE}            { TOKEN(tBEFORE); }
<PSL>{BEFORE}!           { TOKEN(tBEFORE1); }
<PSL>{BEFORE}_           { TOKEN(tBEFORE_); }
<PSL>{BEFORE}!_          { TOKEN(tBEFORE1_); }
<PSL>"|->"               { TOKEN(tSUFFIXOVR); }
<PSL>"|=>"               { TOKEN(tSUFFIXNON); }
<PSL>{INF}               { TOKEN(tINF); }
<PSL>{ENDPOINT}          { TOKEN(tENDPOINT); }

<VLOG>"module"           { return tMODULE; }
<VLOG>"endmodule"        { return tENDMODULE; }
<VLOG>"input"            { return tINPUT; }
<VLOG>"output"           { return tOUTPUT; }
<VLOG>"inout"            { return tINOUT; }
<VLOG>"reg"              { return tREG; }
<VLOG>"always"           { return tALWAYS; }
<VLOG>"posedge"          { return tPOSEDGE; }
<VLOG>"negedge"          { return tNEGEDGE; }
<VLOG>"initial"          { return tINITIAL; }
<VLOG>"begin"            { return tBEGIN; }
<VLOG>"end"              { return tEND; }
<VLOG>"wire"             { return tWIRE; }
<VLOG>"assign"           { return tASSIGN; }
<VLOG>"if"               { return tIF; }
<VLOG>"else"             { return tELSE; }
<VLOG>"`timescale"       { return tTIMESCALE; }
<VLOG>"`celldefine"      { }
<VLOG>"`endcelldefine"   { }
<VLOG>"`begin_keywords"  { return tBEGINKEYWORDS; }
<VLOG>"`end_keywords"    { return tENDKEYWORDS; }
<VLOG>"supply0"          { return tSUPPLY0; }
<VLOG>"supply1"          { return tSUPPLY1; }
<VLOG>"pulldown"         { return tPULLDOWN; }
<VLOG>"pullup"           { return tPULLUP; }
<VLOG>"forever"          { return tFOREVER; }
<VLOG>"=="               { return tLOGEQ; }
<VLOG>"!="               { return tLOGNEQ; }
<VLOG>"==="              { return tCASEEQ; }
<VLOG>"!=="              { return tCASENEQ; }
<VLOG>"||"               { return tLOGOR; }
<VLOG>"&&"               { return tDBLAMP; }
<VLOG>"<<"               { return tSHIFTLL; }
<VLOG>">>"               { return tSHIFTRL; }
<VLOG>"<<<"              { return tSHIFTLA; }
<VLOG>">>>"              { return tSHIFTRA; }
<VLOG>"(*"               { return tATTRBEGIN; }
<VLOG>"*)"               { return tATTREND; }
<VLOG>"%"                { return tPERCENT; }
<VLOG>"specify"          { return tSPECIFY; }
<VLOG>"endspecify"       { return tENDSPECIFY; }
<VLOG>"primitive"        { return tPRIMITIVE; }
<VLOG>"endprimitive"     { return tENDPRIMITIVE; }
<VLOG>"table"            { return tTABLE; }
<VLOG,UDP>"endtable"     { return tENDTABLE; }
<VLOG>"and"              { return tAND; }
<VLOG>"nand"             { return tNAND; }
<VLOG>"or"               { return tOR; }
<VLOG>"nor"              { return tNOR; }
<VLOG>"xor"              { return tXOR; }
<VLOG>"xnor"             { return tXNOR; }
<VLOG>"not"              { return tNOT; }
<VLOG>"buf"              { return tBUF; }
<VLOG>"struct"           { return tSTRUCT; }
<VLOG>"packed"           { return tPACKED; }
<VLOG>"void"             { return tVOID; }
<VLOG>"byte"             { return tBYTE; }
<VLOG>"shortint"         { return tSHORTINT; }
<VLOG>"longint"          { return tLONGINT; }
<VLOG>"int"              { return tSVINT; }
<VLOG>"integer"          { return tINTEGER; }
<VLOG>"real"             { return tSVREAL; }
<VLOG>"shortreal"        { return tSHORTREAL; }
<VLOG>"realtime"         { return tREALTIME; }
<VLOG>"time"             { return tTIME; }
<VLOG>"typedef"          { return tTYPEDEF; }
<VLOG>"bit"              { return tBIT; }
<VLOG>"logic"            { return tLOGIC; }
<VLOG>"enum"             { return tENUM; }
<VLOG>"union"            { return tUNION; }
<VLOG>"tagged"           { return tTAGGED; }
<VLOG>"while"            { return tWHILE; }
<VLOG>"repeat"           { return tREPEAT; }
<VLOG>"do"               { return tDO; }
<VLOG>"task"             { return tTASK; }
<VLOG>"endtask"          { return tENDTASK; }
<VLOG>"function"         { return tFUNCTION; }
<VLOG>"endfunction"      { return tENDFUNCTION; }
<VLOG>"wait"             { return tWAIT; }
<VLOG>"parameter"        { return tPARAMETER; }

<VLOG>{SYSTASK}          { yylval.str = xstrdup(yytext); return tSYSTASK; }
<VLOG>{VLOG_ID}          { yylval.str = xstrdup(yytext); return tID; }
<VLOG>{INTEGER}          { yylval.str = xstrdup(yytext); return tUNSIGNED; }
<VLOG>{VLOG_STRING}      { return escape_verilog_string(yytext); }
<VLOG>{VLOG_NUMBER}      { yylval.str = xstrdup(yytext); return tNUMBER; }
<VLOG>{VLOG_REAL}        { return parse_verilog_real(yytext); };

<INITIAL,PSL>{DECIMAL}   { return parse_decimal_literal(yytext); }
<INITIAL,PSL>{BASED}     { return parse_based_literal(yytext); }
<INITIAL,PSL>{BITSTRING} { return parse_bit_string(yytext); }
<INITIAL,PSL>{STRING}    { return parse_string(yytext); }
<INITIAL,PSL>{BADSTRING} { return report_unterminated_string(yytext); }
<INITIAL,PSL>{CHAR}      { if (resolve_ir1045()) {
                              yylval.ident = ident_new(yytext);
                              TOKEN(tID);
                           }
                           REJECT;
                         }

% /* These PSL tokens must be scanned in VHDL mode for look-ahead */
"->"                     { TOKEN(tIFIMPL); }
"<->"                    { TOKEN(tIFFIMPL); }
"[*"                     { TOKEN(tTIMESRPT); }
"[+]"                    { TOKEN(tPLUSRPT); }
"[="                     { TOKEN(tEQRPT); }
"[->"                    { TOKEN(tGOTORPT); }
{UNTIL}!                 { TOKEN(tUNTIL1); }
{UNTIL}_                 { TOKEN(tUNTIL_); }
{UNTIL}!_                { TOKEN(tUNTIL1_); }

% /* These VHDL tokens must be scanned in PSL mode for look-ahead */
<PSL>{SEVERITY}          { TOKEN(tSEVERITY); }
<PSL>{REPORT}            { TOKEN(tREPORT); }


% /* SDF rules separated from rest for better maintainability */

% /* SDF hierarchical_identifier may conflict with SDF keywords ->
     Must be scanned in separate mode! Tokens that may be lookahead
     after an identifier must be scanned too. */
<SDF,SDF_EXPR>"!"       { TOKEN(tBANG); }
<SDF,SDF_EXPR>{TICK}    { TOKEN(tTICK); }
<SDF,SDF_EXPR>"<<"      { TOKEN(tLTLT); }
<SDF,SDF_EXPR>">>"      { TOKEN(tGTGT); }
<SDF,SDF_EXPR>"<="      { TOKEN(tLE); }
<SDF,SDF_EXPR>">="      { TOKEN(tGE); }
<SDF,SDF_EXPR>"&&"      { TOKEN(tDBLAMP); }
<SDF,SDF_EXPR>"~&"      { TOKEN(tTILDEAMP); }
<SDF,SDF_EXPR>"~|"      { TOKEN(tTILDEBAR); }
<SDF,SDF_EXPR>"^~"      { TOKEN(tTILDECARET); }
<SDF,SDF_EXPR>"~^"      { TOKEN(tTILDECARET); }
<SDF,SDF_EXPR>"%"       { TOKEN(tPERCENT); }
<SDF,SDF_EXPR>"||"      { TOKEN(tLOGOR); }
<SDF,SDF_EXPR>"^"       { TOKEN(tCARET); }
<SDF,SDF_EXPR>"=="      { TOKEN(tLOGEQ); }
<SDF,SDF_EXPR>"!="      { TOKEN(tLOGNEQ); }
<SDF,SDF_EXPR>"==="     { TOKEN(tCASEEQ); }
<SDF,SDF_EXPR>"!=="     { TOKEN(tCASENEQ); }

<SDF,SDF_EXPR>"//"      { comment_caller = YY_START; BEGIN(COMMENT); }
<SDF,SDF_EXPR>"/*"      { comment_caller = YY_START;
                          BEGIN(C_COMMENT);
                        }

<SDF,SDF_EXPR>{SCALAR_ZERO}   { TOKEN(tSCALARZERO); }
<SDF,SDF_EXPR>{SCALAR_ONE}    { TOKEN(tSCALARONE); }

% /* TODO: Crosscheck SDF definition of decimal */
<SDF,SDF_EXPR>{DECIMAL} { return parse_decimal_literal(yytext); }

% /* VLOG_STRING is defined almost equally as SDF qstring */
<SDF>{VLOG_STRING}     { return parse_string(yytext); }

<SDF>{DELAYFILE}        { TOKEN(tDELAYFILE); }
<SDF>{SDFVERSION}       { TOKEN(tSDFVERSION); }
<SDF>{DESIGN}           { TOKEN(tDESIGN); }
<SDF>{DATE}             { TOKEN(tDATE); }
<SDF>{VENDOR}           { TOKEN(tVENDOR); }
<SDF>{PROGRAM}          { TOKEN(tPROGRAM); }
<SDF>{VERSION}          { TOKEN(tVERSION); }
<SDF>{DIVIDER}          { TOKEN(tDIVIDER); }
<SDF>{VOLTAGE}          { TOKEN(tVOLTAGE); }
<SDF>{PROCESS}          { TOKEN(tPROCESS); }
<SDF>{TEMPERATURE}      { TOKEN(tTEMPERATURE); }
<SDF>{TIMESCALE}        { TOKEN(tTIMESCALE); }
<SDF>{CELL}             { TOKEN(tCELL); }
<SDF>{CELLTYPE}         { TOKEN(tCELLTYPE); }
<SDF>{INSTANCE}         { TOKEN(tINSTANCE); }
<SDF>{DELAY}            { TOKEN(tDELAY); }
<SDF>{ABSOLUTE}         { TOKEN(tABSOLUTE); }
<SDF>{PATHPULSE}        { TOKEN(tPATHPULSE); }
<SDF>{PATHPULSEP}       { TOKEN(tPATHPULSEP); }
<SDF>{INCREMENT}        { TOKEN(tINCREMENT); }
<SDF>{IOPATH}           { TOKEN(tIOPATH); }
<SDF>"posedge"          { TOKEN(tPOSEDGE); }
<SDF>"negedge"          { TOKEN(tNEGEDGE); }
<SDF>{SETUP}            { TOKEN(tSETUP); }
<SDF>{HOLD}             { TOKEN(tHOLD); }
<SDF>{SETUPHOLD}        { TOKEN(tSETUPHOLD); }
<SDF>{RECOVERY}         { TOKEN(tRECOVERY); }
<SDF>{REMOVAL}          { TOKEN(tREMOVAL); }
<SDF>{RECREM}           { TOKEN(tRECREM); }
<SDF>{WIDTH}            { TOKEN(tWIDTH); }
<SDF>{PERIOD}           { TOKEN(tPERIOD); }
<SDF>{SKEW}             { TOKEN(tSKEW); }
<SDF>{BIDIRSKEW}        { TOKEN(tBIDIRSKEW); }
<SDF>{NOCHANGE}         { TOKEN(tNOCHANGE); }
<SDF>{PORT}             { TOKEN(tPORT); }
<SDF>{INTERCONNECT}     { TOKEN(tINTERCONNECT); }
<SDF>{NETDELAY}         { TOKEN(tNETDELAY); }
<SDF>{DEVICE}           { TOKEN(tDEVICE); }
<SDF>{COND}             { TOKEN(tSDFCOND); }
<SDF>{CONDELSE}         { TOKEN(tSDFCONDELSE); }
<SDF>{PATHCONSTRAINT}   { TOKEN(tPATHCONSTR); }
<SDF>{PERIODCONSTRAINT} { TOKEN(tPERIODCONSTR); }
<SDF>{SUM}              { TOKEN(tSUM); }
<SDF>{DIFF}             { TOKEN(tDIFF); }
<SDF>{SKEWCONSTRAINT}   { TOKEN(tSKEWCONSTR); }
<SDF>{ARRIVAL}          { TOKEN(tARRIVAL); }
<SDF>{DEPARTURE}        { TOKEN(tDEPARTURE); }
<SDF>{SLACK}            { TOKEN(tSLACK); }
<SDF>{WAVEFORM}         { TOKEN(tWAVEFORM); }
<SDF>{NAME}             { TOKEN(tNAME); }
<SDF>{EXCEPTION}        { TOKEN(tEXCEPTION); }
<SDF>{LABEL}            { TOKEN(tLABEL); }
<SDF>{TIMINGCHECK}      { TOKEN(tTIMINGCHECK); }
<SDF>{TIMINGENV}        { TOKEN(tTIMINGENV); }
<SDF>{RETAIN}           { TOKEN(tRETAIN); }
<SDF>{SCOND}            { TOKEN(tSCOND); }
<SDF>{CCOND}            { TOKEN(tCCOND); }

<SDF,SDF_EXPR>{SDF_ID}  { yylval.str = xstrdup(yytext); TOKEN(tID); }


<INITIAL,PSL>{UTF8_MB}   { warn_utf8(yytext); REJECT; }

<INITIAL,PSL>{VHDL_ID}   { return parse_id(yytext); }
{EXID}                   { return parse_ex_id(yytext); }
<INITIAL,PSL>{BAD_ID}    { return report_bad_identifier(yytext); }
<*>{SPACE}               { }
<*>"\n"                  { /* Must match a single character */ }
<*><<EOF>>               { return 0; }
<*>.                     { TOKEN(tERROR); }

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

static bool begin_psl_comment(void)
{
   static bool printed = false;

   if (opt_get_int(OPT_PSL_COMMENTS))
      return true;
   else if (!printed) {
      // TODO: enable this once PSL support is complete
      // note_at(&yylloc, "pass $bold$--psl$$ to enable parsing of PSL "
      //   "directives in comments");
      printed = true;
      return false;
   }
   else
      return false;
}

static int parse_id(char *str)
{
   for (char *p = str; *p; p++)
      *p = toupper_iso88591(*p);

   yylval.ident = ident_new(str);
   TOKEN(tID);
}

static int parse_ex_id(char *str)
{
   // Replace double '\\' character by single '\\'
   char *p = str + 1;
   for (char *s = p; *s; ) {
      if (*s == '\\' && *(s + 1) == '\\') s++;
      *p++ = *s++;
   }
   *p = '\0';

   yylval.ident = ident_new(str);
   TOKEN(tID);
}

static void strip_underscores(char *s)
{
   bool error = false;
   char *p;
   for (p = s; *s; s++) {
      if (*s != '_')
         *p++ = *s;
      else if ((s[1] == '\0' || s[1] == '_') && !error) {
         error_at(&yylloc, "a literal may not end with an underscore or "
                  "contain multiple consecutive underscores");
         error = true;
      }
   }
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

      errno = 0;
      yylval.i64 = strtoll(val, NULL, 10);
      bool overflow = (errno == ERANGE);

      long long int e = (exp ? atoll(exp) : 0);

      if (yylval.i64 == 0)
         tok = tINT;
      else if (e >= 0) {  // Minus sign forbidden for an integer literal
         for (; e > 0; e--) {
            if (__builtin_mul_overflow(yylval.i64, INT64_C(10), &yylval.i64)) {
               overflow = true;
               break;
            }
         }
         tok = (sign == NULL) ? tINT : tERROR;
      }

      if (overflow)
         error_at(&yylloc, "value %s is outside implementation defined range "
                  "of universal_integer", str);
   }
   else {
      yylval.real = strtod(tmp, NULL);
      tok = tREAL;
   }

   TOKEN(tok);
}

static int parse_based_literal(const char *str)
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
         yylval.i64 = strtoll(val, &eptr, atoi(base));

         long long int e = (exp ? atoll(exp) : 0);

         if (e >= 0) {  // Minus sign forbidden for an integer literal
            for (; e > 0; e--) yylval.i64 *= atoi(base);
            tok = ((*eptr == '\0') && (sign == NULL)) ? tINT : tERROR;
         }
      }
      else {
         char *eptr_integer, *eptr_rational;
         char *integer  = strtok(val, ".");
         char *rational = strtok(NULL, ".");

         yylval.real = (double)strtoll(integer, &eptr_integer, atoi(base));

         double tmp = (double)strtoll(rational, &eptr_rational, atoi(base));
         tmp *= pow((double)atoi(base), (double)((long)(0 - strlen(rational))));

         yylval.real += tmp;

         long long int e = (exp ? atoll(exp) : 0);

         if (e != 0)
            yylval.real *= pow((double) atoi(base), (double) e);

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

   char *s = (yylval.str = xstrdup(str));

   // Replacing double '\"' character by single '\"' or double '%'
   // character by single '%'
   // Begins after first character
   s++;
   char *p = s;
   while (*p) {
      if ((*p == *(yylval.str)) && (*(p+1) == *(yylval.str))) p++;
      *s++ = *p++;
   }
   *s = *p;

   TOKEN(tSTRING);
}

static int parse_bit_string(const char *str)
{
   // Copy input, remove all '_' characters and replace all '\%'
   // characters by '\"'.

   char *copy = (yylval.str = xstrdup(str));

   bool error = false;
   for (char *src = copy, *dst = src; *src != '\0'; src++) {
      if (*src == '%')
         *dst++ = '\"';
      else if (*src != '_')
         *dst++ = *src;
      else if ((src[-1] == '\"' || src[1] == '\"') && !error) {
         error_at(&yylloc, "a bit string literal may not start or end with "
                  "an underscore");
         error = true;
      }
      else if (src[-1] == '_' && !error) {
         error_at(&yylloc, "a bit string literal may not contain multiple "
                  "consecutive underscores");
         error = true;
      }
   }

   TOKEN(tBITSTRING);
}

static int parse_verilog_real(const char *str)
{
   // unsigned_number [ . unsigned_number ] exp [ sign ] unsigned_number

   yylval.real = strtod(str, NULL);
   return tREAL;
}

static int report_unterminated_string(const char *str)
{
   diag_t *d = diag_new(DIAG_ERROR, &yylloc);
   diag_printf(d, "unterminated string literal");
   diag_hint(d, NULL, "a string literal must fit on one line since it "
             "is a lexical element, longer strings can be formed using "
             "concatenation");
   diag_lrm(d, STD_08, "15.7");
   diag_emit(d);

   return parse_string(str);
}

static int report_bad_identifier(char *str)
{
   diag_t *d = diag_new(DIAG_ERROR, &yylloc);
   diag_printf(d, "a VHDL basic identifier must not start or end with an "
               "underscore, or contain multiple consecutive underscores");
   diag_lrm(d, STD_08, "15.4.2");
   diag_emit(d);

   return parse_id(str);
}

static int escape_verilog_string(const char *str)
{
   char *s = (yylval.str = xstrdup(str));
   for (char *p = s; *p; p++) {
      if (*p == '\\') {
         switch (*++p) {
         case 'n': *s++ = '\n'; break;
         case 'r': *s++ = '\r'; break;
         case 't': *s++ = '\t'; break;
         case 'b': *s++ = '\b'; break;
         default:
            warn_at(&yylloc, "unrecognised escaped character '%c'", *p);
            *s++ = *p;
         }
      }
      else
         *s++ = *p;
   }
   *s = '\0';

   return tSTRING;
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

static void warn_utf8(const char *text)
{
   static bool warned = false;

   if (!warned) {
      diag_t *d = diag_new(DIAG_WARN, &yylloc);
      diag_printf(d, "possible multi-byte UTF-8 character found in input");
      diag_hint(d, NULL, "the native encoding of VHDL is ISO-8859-1");
      diag_emit(d);

      warned = true;
   }
}

void reset_scanner(void)
{
   YY_FLUSH_BUFFER;
   BEGIN(INITIAL);
}

void scan_as_psl(void)
{
   BEGIN(PSL);
}

void scan_as_vhdl(void)
{
   BEGIN(INITIAL);
}

void scan_as_verilog(void)
{
   BEGIN(VLOG);
}

void scan_as_udp(void)
{
   BEGIN(UDP);
}

void scan_as_sdf(void)
{
   BEGIN(SDF);
}

void scan_as_sdf_expr(void)
{
   BEGIN(SDF_EXPR);
}

bool is_scanned_as_psl(void)
{
   return (YY_START == PSL);
}
