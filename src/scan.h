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

#ifndef _SCAN_H
#define _SCAN_H

#include "prim.h"

typedef struct _node_list node_list_t;
typedef struct { ident_t left, right; } ident_pair_t;

typedef union {
   double        real;
   char         *str;
   int64_t       i64;
   unsigned      kind;
   ident_pair_t  pair;
   ident_t       ident;
   node_list_t  *list;
   vlog_node_t   vlog;
} yylval_t;

// Functions shared between VHDL and Verilog scanners

typedef enum { SOURCE_VHDL, SOURCE_VERILOG } hdl_kind_t;

typedef int token_t;

void input_from_file(const char *file);
hdl_kind_t source_kind(void);
token_t processed_yylex(void);
const char *token_str(token_t tok);

void pp_defines_add(const char *ident, const char *value);

void scan_as_psl(void);
void scan_as_vhdl(void);
void scan_as_verilog(void);

// Private interface to Flex scanners

void begin_token(char *tok, int length);
int get_next_char(char *b, int max_buffer);

void reset_vhdl_parser(void);
void reset_verilog_parser(void);

#define tEOF           0

#define tLPAREN        '('
#define tRPAREN        ')'
#define tSEMI          ';'
#define tCOLON         ':'
#define tCOMMA         ','
#define tAT            '@'
#define tLT            '<'
#define tGT            '>'
#define tPLUS          '+'
#define tMINUS         '-'
#define tTIMES         '*'
#define tDOT           '.'
#define tTICK          '\''
#define tEQ            '='
#define tAMP           '&'
#define tLBRACE        '{'
#define tRBRACE        '}'
#define tCARET         '^'
#define tQUESTION      '?'
#define tBAR           '|'
#define tLSQUARE       '['
#define tRSQUARE       ']'
#define tOVER          '/'

#define tID            200
#define tENTITY        201
#define tIS            202
#define tEND           203
#define tGENERIC       204
#define tPORT          205
#define tCONSTANT      206
#define tCOMPONENT     207
#define tCONFIGURATION 208
#define tARCHITECTURE  209
#define tOF            210
#define tBEGIN         211
#define tFOR           212
#define tTYPE          213
#define tTO            214
#define tALL           215
#define tIN            216
#define tOUT           217
#define tBUFFER        218
#define tBUS           219
#define tUNAFFECTED    220
#define tSIGNAL        221
#define tDOWNTO        222
#define tPROCESS       223
#define tPOSTPONED     224
#define tWAIT          225
#define tREPORT        226
#define tASSIGN        227
#define tINT           228
#define tSTRING        229
#define tERROR         230
#define tINOUT         231
#define tLINKAGE       232
#define tVARIABLE      233
#define tIF            234
#define tRANGE         235
#define tSUBTYPE       236
#define tUNITS         237
#define tPACKAGE       238
#define tLIBRARY       239
#define tUSE           240
#define tNULL          241
#define tFUNCTION      242
#define tIMPURE        243
#define tRETURN        244
#define tPURE          245
#define tARRAY         246
#define tBOX           247
#define tASSOC         248
#define tOTHERS        249
#define tASSERT        250
#define tSEVERITY      251
#define tON            252
#define tMAP           253
#define tTHEN          254
#define tELSE          255
#define tELSIF         256
#define tBODY          257
#define tWHILE         258
#define tLOOP          259
#define tAFTER         260
#define tALIAS         261
#define tATTRIBUTE     262
#define tPROCEDURE     263
#define tEXIT          264
#define tNEXT          265
#define tWHEN          266
#define tCASE          267
#define tLABEL         268
#define tGROUP         269
#define tLITERAL       270
#define tINERTIAL      271
#define tTRANSPORT     272
#define tREJECT        273
#define tBITSTRING     274
#define tBLOCK         275
#define tWITH          276
#define tSELECT        277
#define tGENERATE      278
#define tACCESS        279
#define tFILE          280
#define tOPEN          281
#define tREAL          282
#define tUNTIL         283
#define tRECORD        284
#define tNEW           285
#define tSHARED        286
#define tAND           287
#define tOR            288
#define tNAND          289
#define tNOR           290
#define tXOR           291
#define tXNOR          292
#define tNEQ           293
#define tLE            294
#define tGE            295
#define tPOWER         296
#define tSLL           297
#define tSRL           298
#define tSLA           299
#define tSRA           300
#define tROL           301
#define tROR           302
#define tMOD           303
#define tREM           304
#define tABS           305
#define tNOT           306
#define tGUARDED       307
#define tREVRANGE      308
#define tPROTECTED     309
#define tCONTEXT       310
#define tCONDIF        311
#define tCONDELSE      312
#define tCONDELSIF     313
#define tCONDEND       314
#define tCONDERROR     315
#define tCONDWARN      316
#define tSYNTHOFF      317
#define tSYNTHON       318
#define tMEQ           319
#define tMNEQ          320
#define tMLT           321
#define tMLE           322
#define tMGT           323
#define tMGE           324
#define tREGISTER      325
#define tDISCONNECT    326
#define tCCONV         327
#define tLTLT          328
#define tGTGT          329
#define tFORCE         330
#define tRELEASE       331
#define tPARAMETER     332
#define tCOVERAGEON    333
#define tCOVERAGEOFF   334
#define tSTARTPSL      335
#define tALWAYS        336
#define tIFIMPL        337
#define tIFFIMPL       338
#define tDEFAULT       339
#define tCLOCK         340
#define tNEXT1         341
#define tNEVER         342
#define tEVENTUALLY    343
#define tNEXTA         344
#define tNEXTA1        345
#define tNEXTE         346
#define tNEXTE1        347
#define tNEXTEVENT     348
#define tNEXTEVENT1    349
#define tMODULE        350
#define tENDMODULE     351
#define tINPUT         352
#define tOUTPUT        353
#define tREG           354
#define tPOSEDGE       355
#define tNEGEDGE       356
#define tINITIAL       357
#define tWIRE          358
#define tUNSIGNED      359
#define tASSUME        360
#define tASSUMEG       361
#define tRESTRICT      362
#define tRESTRICTG     363
#define tSTRONG        364
#define tFAIRNESS      365
#define tCOVER         366
#define tPROPERTY      367
#define tSEQUENCE      368
#define tCONST         369
#define tMUTABLE       370
#define tHDLTYPE       371
#define tBOOLEAN       372
#define tBIT           373
#define tBITVECTOR     374
#define tNUMERIC       375
#define tSTRINGK       376
#define tTIMESRPT      377
#define tPLUSRPT       378
#define tGOTORPT       379
#define tARROWRPT      380
#define tDBLAMP        381
#define tWITHIN        382
#define tSYSTASK       383

#endif  // _SCAN_H
