//
//  Copyright (C) 2022-2025  Nick Gasson
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
#include "diag.h"

typedef union {
   double      real;
   char       *str;
   int64_t     i64;
   ident_t     ident;
   text_buf_t *text;
} yylval_t;

// Functions shared between VHDL and Verilog scanners

typedef enum {
   SOURCE_VHDL,
   SOURCE_VERILOG,
   SOURCE_SDF
} hdl_kind_t;

typedef int token_t;

typedef enum {
   VLOG_1364_1995,
   VLOG_1364_2001_NOCONFIG,
   VLOG_1364_2001,
   VLOG_1364_2005,
   VLOG_1800_2005,
   VLOG_1800_2009,
   VLOG_1800_2012,
   VLOG_1800_2017,
   VLOG_1800_2023,
} vlog_version_t;

void input_from_file(const char *file);
void input_from_buffer(const char *buf, size_t len, file_ref_t file_ref,
                       hdl_kind_t hdl);
void push_buffer(const char *buf, size_t len, file_ref_t file_ref);
void push_file(const char *file, const loc_t *srcloc);
void pop_buffer(void);
hdl_kind_t source_kind(void);
token_t processed_yylex(void);
const char *token_str(token_t tok);
void free_token(token_t tok, yylval_t *lval);

typedef void (*pp_iter_t)(const char *, const char *, void *);

void pp_defines_add(const char *name, const char *value);
const char *pp_defines_get(const char *name);
void pp_defines_iter(pp_iter_t fn, void *ctx);

void add_include_dir(const char *str);

void scan_as_psl(void);
void scan_as_vhdl(void);
void scan_as_verilog(void);
void scan_as_udp(void);
void scan_as_sdf(void);
void scan_as_sdf_expr(void);

const char *verilog_version_string(vlog_version_t vers);
bool parse_verilog_version(const char *str, vlog_version_t *vers);

void set_default_keywords(vlog_version_t vers);
void push_keywords(vlog_version_t vers);
bool pop_keywords(void);
bool get_verilog_keywords(vlog_version_t *vers);

// Private interface to Flex scanners

void begin_token(char *tok, int length);
int get_next_char(char *b, int max_buffer);

void reset_vhdl_parser(void);
void reset_verilog_parser(void);
void reset_sdf_parser(void);

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
#define tHASH          '#'
#define tTILDE         '~'
#define tBANG          '!'
#define tPERCENT       '%'

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
#define tWALRUS        227
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
#define tUNSNUM        359
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
#define tEQRPT         380
#define tDBLAMP        381
#define tWITHIN        382
#define tSYSTASK       383
#define tVIEW          384
#define tPRIVATE       385
#define tPREV          386
#define tSTABLE        387
#define tROSE          388
#define tFELL          389
#define tENDED         390
#define tNONDET        391
#define tNONDETV       392
#define tUNION         393
#define tTRANSLATEON   394
#define tTRANSLATEOFF  395
#define tUNTIL1        396
#define tUNTIL_        397
#define tUNTIL1_       398
#define tTIMESCALE     399
#define tSUPPLY0       400
#define tSUPPLY1       401
#define tPULLDOWN      402
#define tPULLUP        403
#define tCASEEQ        404
#define tCASENEQ       405
#define tLOGEQ         406
#define tLOGNEQ        407
#define tATTRBEGIN     408
#define tATTREND       409
#define tNUMBER        410
#define tFOREVER       411
#define t2LSQUARE      412
#define t2RSQUARE      413
#define tSPECIFY       414
#define tENDSPECIFY    415
#define tPRIMITIVE     416
#define tENDPRIMITIVE  417
#define tTABLE         418
#define tENDTABLE      419
#define tASSIGN        420
#define tUDPLEVEL      421
#define tUDPEDGE       422
#define tUDPIND        423
#define tBUF           424
#define tLOGOR         425
#define tSCALARZERO    426
#define tSCALARONE     427
#define tDELAYFILE     428
#define tSDFVERSION    429
#define tDESIGN        430
#define tDATE          431
#define tVENDOR        432
#define tPROGRAM       433
#define tVERSION       434
#define tDIVIDER       435
#define tVOLTAGE       436
#define tTEMPERATURE   437
#define tCELL          438
#define tCELLTYPE      439
#define tINSTANCE      440
#define tDELAY         441
#define tTIMINGCHECK   442
#define tTIMINGENV     443
#define tPATHPULSE     444
#define tPATHPULSEP    445
#define tIOPATH        446
#define tRETAIN        447
#define tSDFCOND       448
#define tSDFCONDELSE   449
#define tINTERCONNECT  450
#define tNETDELAY      451
#define tDEVICE        452
#define tSETUP         453
#define tHOLD          454
#define tSETUPHOLD     455
#define tRECOVERY      456
#define tREMOVAL       457
#define tRECREM        458
#define tSKEW          459
#define tBIDIRSKEW     460
#define tWIDTH         461
#define tPERIOD        462
#define tNOCHANGE      463
#define tCOND          464
#define tSCOND         465
#define tCCOND         466
#define tPATHCONSTR    467
#define tPERIODCONSTR  468
#define tSUM           469
#define tDIFF          470
#define tSKEWCONSTR    471
#define tEXCEPTION     472
#define tNAME          473
#define tARRIVAL       474
#define tDEPARTURE     475
#define tSLACK         476
#define tWAVEFORM      477
#define tINCREMENT     478
#define tABSOLUTE      479
#define tTILDEAMP      480
#define tTILDEBAR      481
#define tTILDECARET    482
#define tSTRUCT        483
#define tPACKED        484
#define tVOID          485
#define tBYTE          486
#define tSHORTINT      487
#define tLONGINT       488
#define tSVINT         489
#define tINTEGER       490
#define tTIME          491
#define tTYPEDEF       492
#define tLOGIC         493
#define tENUM          494
#define tTAGGED        495
#define tABORT         496
#define tSYNC_ABORT    497
#define tASYNC_ABORT   498
#define tBEFORE        499
#define tBEFORE1       500
#define tBEFORE_       501
#define tBEFORE1_      502
#define tSUFFIXOVR     503
#define tSUFFIXNON     504
#define tPSLNEXT       505
#define tINF           506
#define tREPEAT        507
#define tDO            508
#define tENDPOINT      509
#define tSHIFTLL       510
#define tSHIFTRL       511
#define tSHIFTLA       512
#define tSHIFTRA       513
#define tTASK          514
#define tENDTASK       515
#define tENDFUNCTION   516
#define tBEGINKEYWORDS 517
#define tENDKEYWORDS   518
#define tSVREAL        519
#define tSHORTREAL     520
#define tREALTIME      521
#define tNVCPUSH       522
#define tNVCPOP        523
#define tPLUSPLUS      524
#define tMINUSMINUS    525
#define tVAR           526
#define tDEFNETTYPE    527
#define tTRI           528
#define tTRI0          529
#define tTRI1          530
#define tWAND          531
#define tTRIAND        532
#define tWOR           533
#define tTRIOR         534
#define tTRIREG        535
#define tUWIRE         536
#define tNONE          537
#define tLOCALPARAM    538
#define tALWAYSCOMB    539
#define tALWAYSFF      540
#define tALWAYSLATCH   541
#define tPARENSTAR     542
#define tENDCASE       543
#define tCASEX         544
#define tCASEZ         545
#define tIFNONE        546
#define tEDGE          547
#define tTIMESGT       548
#define tDLRSETUP      549
#define tDLRHOLD       550
#define tDLRRECOVERY   551
#define tDLRREMOVAL    552
#define tDLRSETUPHOLD  553
#define tDLRRECREM     554
#define tDLRWIDTH      555
#define tINDEXPOS      556
#define tINDEXNEG      557
#define tENDGENERATE   558
#define tRESETALL      559
#define tEVENT         560
#define tTRPLAMP       561
#define tSPECPARAM     562
#define tFORK          563
#define tJOIN          564
#define tAUTOMATIC     565
#define tGENVAR        566
#define tHIGHZ0        567
#define tHIGHZ1        568
#define tSTRONG0       569
#define tSTRONG1       570
#define tPULL0         571
#define tPULL1         572
#define tWEAK0         573
#define tWEAK1         574
#define tSMALL         575
#define tMEDIUM        576
#define tLARGE         577
#define tVECTORED      578
#define tSCALARED      579
#define tUNCTDRIVE     580
#define tNOUNCTDRIVE   581
#define tDEASSIGN      582
#define tSIGNED        583
#define tUNSIGNED      584
#define tDISABLE       585
#define tCLASS         586
#define tCHANDLE       587
#define tEXPORT        588
#define tIMPORT        589
#define tENDPACKAGE    590
#define tEXTENDS       591
#define tTHIS          592
#define tNETTYPE       593
#define tREF           594
#define tSUPER         595
#define tSTATIC        596
#define tPLUSEQ        597
#define tMINUSEQ       598
#define tTIMESEQ       599
#define tDIVEQ         600
#define tPERCENTEQ     601
#define tAMPEQ         602
#define tBAREQ         603
#define tCARETEQ       604
#define tLSLEQ         605
#define tLSREQ         606
#define tASLEQ         607
#define tASREQ         608
#define tBUFIF0        609
#define tBUFIF1        610
#define tNOTIF0        611
#define tNOTIF1        612
#define tPROTECT       613
#define tBEGINPROT     614
#define tENDPROT       615
#define tENDPROGRAM    616
#define tENDCLASS      617
#define tVIRTUAL       618
#define tSCOPE         619
#define tDEFPARAM      620
#define tTRAN          621
#define tTRANIF0       622
#define tTRANIF1       623
#define tRTRAN         624
#define tRTRANIF0      625
#define tRTRANIF1      626

#endif  // _SCAN_H
