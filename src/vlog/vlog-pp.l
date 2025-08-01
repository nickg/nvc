/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2023-2024  Nick Gasson
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

%option prefix="vlogpp_"
%option noyywrap
%option nounput
%option noinput

%{
#include "util.h"
#include "array.h"
#include "diag.h"
#include "hash.h"
#include "option.h"
#include "scan.h"
#include "vlog/vlog-phase.h"

#include <assert.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION convert_line_ending();

typedef struct _ifdef_stack ifdef_stack_t;

struct _ifdef_stack {
   ifdef_stack_t *next;
   loc_t          loc;
   bool           cond;
};

typedef A(YY_BUFFER_STATE) buffer_stack_t;

static int             comment_caller = 0;
static shash_t        *macros;
static char           *macro_name;
static text_buf_t     *macro_text;
static text_buf_t     *output;
static ifdef_stack_t  *ifdefs = NULL;
static buffer_stack_t  buffer_stack = AINIT;
static bool            emit_locs;
static bool            continuation = false;

static void define_macro(void);
static void undef_macro(void);
static void undefall_macro(void);
static void push_ifdef(bool cond);
static void pop_ifdef(void);
static void convert_line_ending(void);
static void push_macro(const char *name, const char *text);
static void pop_buffer(void);

extern loc_t yylloc;
%}

ID    [a-zA-Z_]([a-zA-Z0-9_$])*

%x COMMENT C_COMMENT MACRO_NAME MACRO_TEXT IFDEF_NAME IFDEF_FALSE ELSIF_NAME
%x IFNDEF_NAME

%%

"//"[^\r\n]*            { tb_cat(output, vlogpp_text); }

"/*"                    { comment_caller = YY_START;
                          tb_cat(output, vlogpp_text);
                          BEGIN(C_COMMENT);
                        }
<C_COMMENT>[^\r\n]      { tb_cat(output, vlogpp_text); }
<C_COMMENT>\n\r         |
<C_COMMENT>\r\n         |
<C_COMMENT>\n           |
<C_COMMENT>\r           { tb_append(output, '\n'); }
<C_COMMENT>"*/"         { tb_cat(output, vlogpp_text);
                          BEGIN(comment_caller);
                        }

`define[ \t]*           { BEGIN(MACRO_NAME); }
`ifdef[ \t]*            { BEGIN(IFDEF_NAME); }
`ifndef[ \t]*           { BEGIN(IFNDEF_NAME); }
`endif                  { pop_ifdef();
                          BEGIN(INITIAL);
                        }
`elsif[ \t]*            { if (ifdefs == NULL)
                             error_at(&yylloc, "`elsif outside of `ifdef");
                          BEGIN(ELSIF_NAME);
                        }
`else                   { if (ifdefs == NULL)
                             error_at(&yylloc, "`else outside of `ifdef");
                          BEGIN(IFDEF_FALSE);
                        }
`undef[ \t]+{ID}[ \t]*  { undef_macro(); }
`undefineall[ \t]*      { undefall_macro(); }

`__FILE__               { tb_printf(output, "\"%s\"", loc_file_str(&yylloc)); }
`__LINE__               { tb_printf(output, "\"%d\"", yylloc.first_line); }

`timescale              |
`default_nettype        |
`resetall               |
`pragma                 |
`unconnected_drive      |
`nounconnected_drive    |
`begin_keywords         |
`end_keywords           |
`celldefine             |
`endcelldefine          { tb_cat(output, vlogpp_text); }

`{ID}                   { const char *text = shash_get(macros, vlogpp_text + 1);
                          if (text != NULL)
                             push_macro(vlogpp_text + 1, text);
                          else
                             warn_at(&yylloc, "macro %s undefined",
                                     vlogpp_text + 1);
                        }

<MACRO_NAME>{ID}[ \t]*  { macro_name = xstrdup(vlogpp_text);
                          tb_rewind(macro_text);
                          BEGIN(MACRO_TEXT); }
<MACRO_NAME>.           { error_at(&yylloc, "expected macro name after "
                                   "`define");
                          BEGIN(INITIAL);
                        }

<IFDEF_NAME>{ID}        { bool cond = (shash_get(macros, vlogpp_text) != NULL);
                          push_ifdef(cond);
                          if (cond == true)
                             BEGIN(INITIAL);
                          else
                             BEGIN(IFDEF_FALSE);
                        }
<IFDEF_NAME>.           { error_at(&yylloc, "expected macro name after `ifdef");
                          BEGIN(INITIAL);
                        }

<IFNDEF_NAME>{ID}       { bool cond = (shash_get(macros, vlogpp_text) == NULL);
                          push_ifdef(cond);
                          if (cond == true)
                             BEGIN(INITIAL);
                          else
                             BEGIN(IFDEF_FALSE);
                        }
<IFNDEF_NAME>.          { error_at(&yylloc, "expected macro name after "
                                   "`ifndef");
                          BEGIN(INITIAL);
                        }

<ELSIF_NAME>{ID}        { if (ifdefs->cond == true)
                             BEGIN(IFDEF_FALSE);
                          else if (shash_get(macros, vlogpp_text) == NULL)
                             BEGIN(IFDEF_FALSE);
                          else
                             BEGIN(INITIAL);
                        }
<ELSIF_NAME>.           { error_at(&yylloc, "expected macro name after `elsif");
                          BEGIN(INITIAL);
                        }

<IFDEF_FALSE>`elsif[ \t]* { assert(ifdefs != NULL);
                            BEGIN(ELSIF_NAME);
                          }

<IFDEF_FALSE>`else      { assert(ifdefs != NULL);
                          BEGIN(INITIAL);
                        }
<IFDEF_FALSE>`endif     { pop_ifdef();
                          BEGIN(INITIAL);
                        }
<IFDEF_FALSE>[^\r\n]    { }
<IFDEF_FALSE>\n\r       |
<IFDEF_FALSE>\r\n       |
<IFDEF_FALSE>\n         |
<IFDEF_FALSE>\r         { tb_append(output, '\n'); }

<MACRO_TEXT>\\          { continuation = true; }
<MACRO_TEXT>[^\r\n]     { if (continuation) {
                             tb_append(macro_text, '\\');
                             continuation = false;
                          }
                          tb_cat(macro_text, vlogpp_text);
                        }
<MACRO_TEXT>\n\r        |
<MACRO_TEXT>\r\n        |
<MACRO_TEXT>\n          |
<MACRO_TEXT>\r          { if (continuation) {
                             tb_append(macro_text, '\n');
                             tb_append(output, '\n');
                             continuation = false;
                          }
                          else {
                             define_macro();
                             tb_cat(output, vlogpp_text);
                             BEGIN(INITIAL);
                          }
                        }

[^\r\n]                 { tb_cat(output, vlogpp_text); }
\n\r                    |
\r\n                    |
\n                      |
\r                      { tb_append(output, '\n'); }

<<EOF>>                 { if (buffer_stack.count > 0)
                             pop_buffer();
                          else
                             yyterminate();
                        }

%%

static void strip_whitespace(char *s)
{
   char *p = s + strlen(s);
   while (p > s && isspace_iso88591(p[-1]))
      *(--p) = '\0';
}

static void convert_line_ending(void)
{
   if (vlogpp_leng >= 2) {
      char *c1 = vlogpp_text + vlogpp_leng - 1;
      char *c2 = vlogpp_text + vlogpp_leng - 2;

      if (*c1 == '\n' && *c2 == '\r') {
         *c1 = '\0';
         *c2 = '\n';
         vlogpp_leng--;
      }
      else if (*c1 == '\r' && *c2 == '\n') {
         *c1 = '\0';
         vlogpp_leng--;
      }
   }

   begin_token(vlogpp_text, vlogpp_leng);
}

static void define_macro(void)
{
   char *copy = xstrdup(tb_get(macro_text));

   strip_whitespace(macro_name);
   strip_whitespace(copy);

   shash_put(macros, macro_name, copy);

   macro_name = NULL;
   tb_rewind(macro_text);
}

static void undef_macro(void)
{
   char *s = yytext + sizeof("`undef");
   while (*s == ' ' || *s == '\t')
      s++;

   strip_whitespace(s);
   shash_delete(macros, s);
}

static void undefall_macro(void)
{
   shash_free(macros);
   macros = shash_new(64);
}


static void push_ifdef(bool cond)
{
   ifdef_stack_t *s = xcalloc(sizeof(ifdef_stack_t));
   s->next = ifdefs;
   s->cond = cond;
   s->loc = yylloc;

   ifdefs = s;
}

static void pop_ifdef(void)
{
   if (ifdefs == NULL)
      error_at(&yylloc, "`endif outside of `ifdef");
   else {
      ifdef_stack_t *tmp = ifdefs->next;
      free(ifdefs);
      ifdefs = tmp;
   }
}

static void push_macro(const char *name, const char *text)
{
   if (emit_locs)
      tb_printf(output, "\n`__nvc_push %s,%d:%d,%d\n", name, yylloc.first_line,
                yylloc.first_column, yylloc.column_delta);

   APUSH(buffer_stack, YY_CURRENT_BUFFER);
   yy_switch_to_buffer(yy_scan_string(text));
}

static void pop_buffer(void)
{
   assert(buffer_stack.count > 0);

   if (emit_locs)
      tb_printf(output, "\n`__nvc_pop\n");

   yy_delete_buffer(YY_CURRENT_BUFFER);
   yy_switch_to_buffer(APOP(buffer_stack));
}

static void vlog_define_cb(const char *key, const char *value, void *ctx)
{
   shash_put(macros, key, (void *)value);
}

void vlog_preprocess(text_buf_t *tb, bool precise)
{
   if (macros == NULL) {
      macros = shash_new(64);
      pp_defines_iter(vlog_define_cb, NULL);
   }
   else
      assert(opt_get_int(OPT_SINGLE_UNIT));

   assert(macro_text == NULL);
   macro_text = tb_new();

   assert(output == NULL);
   output = tb;

   assert(ifdefs == NULL);

   emit_locs = precise;
   continuation = false;

   YY_FLUSH_BUFFER;
   BEGIN(INITIAL);

   vlogpp_lex();
   output = NULL;

   while (ifdefs != NULL) {
      error_at(&(ifdefs->loc), "no corresponding `endif before end of file");
      pop_ifdef();
   }

   if (!opt_get_int(OPT_SINGLE_UNIT)) {
      shash_free(macros);
      macros = NULL;
   }

   free(macro_name);
   macro_name = NULL;

   tb_free(macro_text);
   macro_text = NULL;

   assert(buffer_stack.count == 0);
   ACLEAR(buffer_stack);
}
