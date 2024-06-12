/* -*- mode: c; c-basic-offset: 3 -*- */

/*
 *  Copyright (C) 2023  Nick Gasson
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
#include "diag.h"
#include "hash.h"
#include "scan.h"
#include "vlog/vlog-phase.h"

#include <assert.h>

#define YY_INPUT(buf, result, max_size) {    \
      result = get_next_char(buf, max_size); \
      if (result <= 0)                       \
         result = YY_NULL;                   \
   }

#define YY_USER_ACTION begin_token(vlogpp_text, vlogpp_leng);

static int         comment_caller = 0;
static shash_t    *macros;
static char       *macro_name;
static text_buf_t *macro_text;
static text_buf_t *output;


static void define_macro(void);

extern loc_t yylloc;
%}

ID    [a-zA-Z_]([a-zA-Z0-9_$])*

%x COMMENT C_COMMENT MACRO_NAME MACRO_TEXT

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

`timescale              |
`resetall               |
`pragma                 |
`celldefine             |
`endcelldefine          { tb_cat(output, vlogpp_text); }

`{ID}                   { const char *text = shash_get(macros, vlogpp_text + 1);
                          if (text != NULL)
                             tb_cat(output, text);
                          else
                             warn_at(&yylloc, "macro %s undefined",
                                     vlogpp_text + 1);
                        }

<MACRO_NAME>{ID}[ \t]*  { macro_name = xstrdup(vlogpp_text);
                          tb_rewind(macro_text);
                          BEGIN(MACRO_TEXT); }
<MACRO_NAME>.           { abort(); }

<MACRO_TEXT>[^\r\n]     { tb_cat(macro_text, vlogpp_text); }
<MACRO_TEXT>\n\r        |
<MACRO_TEXT>\r\n        |
<MACRO_TEXT>\n          |
<MACRO_TEXT>\r          { define_macro();
                          tb_cat(output, vlogpp_text);
                          BEGIN(INITIAL);
                        }

[^\r\n]                 { tb_cat(output, vlogpp_text); }
\n\r                    |
\r\n                    |
\n                      |
\r                      { tb_append(output, '\n'); }

%%

static void strip_whitespace(char *s)
{
   char *p = s + strlen(s);
   while (p > s && isspace_iso88591(p[-1]))
      *(--p) = '\0';
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

void vlog_preprocess(text_buf_t *tb)
{
   assert(macros == NULL);
   macros = shash_new(64);

   assert(macro_text == NULL);
   macro_text = tb_new();

   assert(output == NULL);
   output = tb;

   YY_FLUSH_BUFFER;
   BEGIN(INITIAL);

   vlogpp_lex();
   output = NULL;

   shash_free(macros);
   macros = NULL;

   free(macro_name);
   macro_name = NULL;

   tb_free(macro_text);
   macro_text = NULL;
}

