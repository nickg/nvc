//
//  Copyright (C) 2014-2024  Nick Gasson
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

#include "util.h"
#include "array.h"
#include "diag.h"
#include "ident.h"
#include "option.h"
#include "scan.h"
#include "hash.h"

#include <assert.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>

typedef struct {
   bool  result;
   bool  taken;
   loc_t loc;
} cond_state_t;

typedef A(cond_state_t) cond_stack_t;

static const char     *file_start;
static size_t          file_sz;
static const char     *read_ptr;
static hdl_kind_t      src_kind;
static loc_file_ref_t  file_ref = FILE_INVALID;
static int             colno;
static int             lineno;
static int             lookahead;
static int             pperrors;
static cond_stack_t    cond_stack;
static shash_t        *pp_defines;

extern int yylex(void);
extern yylval_t yylval;
extern loc_t yylloc;

extern void reset_scanner(void);

static bool pp_cond_analysis_expr(void);
static void pp_defines_init();

#ifndef ENABLE_VERILOG
yylval_t yylval;
loc_t yylloc;
#endif

void input_from_buffer(const char *buf, size_t len, hdl_kind_t kind)
{
   pp_defines_init();

   reset_scanner();

   yylloc = LOC_INVALID;

   file_start = buf;
   file_sz    = len;
   src_kind   = kind;
   read_ptr   = buf;
   lineno     = 1;
   colno      = 0;
   lookahead  = -1;
   pperrors   = 0;

   switch (kind) {
   case SOURCE_VERILOG:
#ifdef ENABLE_VERILOG
      reset_verilog_parser();
#endif
      break;
   case SOURCE_VHDL:
      reset_vhdl_parser();
      break;
   }
}

void input_from_file(const char *file)
{
   // TODO: need a more sophisticated mechanism to determine HDL type
   hdl_kind_t kind = SOURCE_VHDL;
   size_t len = strlen(file);
   if (len > 2 && file[len - 2] == '.' && file[len - 1] == 'v')
      kind = SOURCE_VERILOG;

   int fd;
   if (strcmp(file, "-") == 0)
      fd = STDIN_FILENO;
   else {
      fd = open(file, O_RDONLY);
      if (fd < 0)
         fatal_errno("opening %s", file);
   }

   file_info_t info;
   if (!get_handle_info(fd, &info))
      fatal_errno("%s: cannot get file info", file);

   if (info.type == FILE_FIFO) {
      // Read all the data from the pipe into a buffer
      size_t bufsz = 16384, total = 0;
      char *buf = xmalloc(bufsz);
      int nbytes;
      do {
         if (bufsz - 1 - file_sz == 0)
            buf = xrealloc(buf, bufsz *= 2);

         nbytes = read(fd, buf + total, bufsz - 1 - total);
         if (nbytes < 0)
            fatal_errno("read");

         total += nbytes;
         buf[total] = '\0';
      } while (nbytes > 0);

      input_from_buffer(buf, total, kind);
   }
   else if (info.type == FILE_REGULAR) {
      void *map = NULL;
      if (info.size > 0)
         map = map_file(fd, info.size);

      file_ref = loc_file_ref(file, map);

      input_from_buffer(map, info.size, kind);
   }
   else
      fatal("opening %s: not a regular file", file);

   close(fd);
}

hdl_kind_t source_kind(void)
{
   return src_kind;
}

int get_next_char(char *b, int max_buffer)
{
   const ptrdiff_t navail = file_start + file_sz - read_ptr;
   assert(navail >= 0);

   if (navail == 0)
      return 0;

   const int nchars = MIN(navail, max_buffer);

   memcpy(b, read_ptr, nchars);
   read_ptr += nchars;

   return nchars;
}

void begin_token(char *tok, int length)
{
   // Newline must match as a single token for the logic below to work
   assert(strchr(tok, '\n') == NULL || length == 1);

   const int first_col = colno;
   if (*tok == '\n') {
      colno = 0;
      lineno += 1;
   }
   else
      colno += length;

   const int last_col = first_col + length - 1;

   extern loc_t yylloc;
   yylloc = get_loc(lineno, first_col, lineno, last_col, file_ref);
}

const char *token_str(token_t tok)
{
   if (tok == tEOF)
      return "end of file";
   else if (tok < 128) {
      static char buf[2];
      buf[0] = tok;
      return buf;
   }
   else {
      static const char *token_strs[] = {
         "identifier", "entity", "is", "end", "generic", "port", "constant",
         "component", "configuration", "architecture", "of", "begin", "for",
         "type", "to", "all", "in", "out", "buffer", "bus", "unaffected",
         "signal", "downto", "process", "postponed", "wait", "report", ":=",
         "integer", "string", "error", "inout", "linkage", "variable", "if",
         "range", "subtype", "units", "package", "library", "use", "null",
         "function", "impure", "return", "pure", "array", "<>", "=>", "others",
         "assert", "severity", "on", "map", "then", "else", "elsif", "body",
         "while", "loop", "after", "alias", "attribute", "procedure", "exit",
         "next", "when", "case", "label", "group", "literal", "inertial",
         "transport", "reject", "bit string", "block", "with", "select",
         "generate", "access", "file", "open", "real", "until", "record",
         "new", "shared", "and", "or", "nand", "nor", "xor", "xnor", "/=",
         "<=", ">=", "**", "sll", "srl", "sla", "sra", "rol", "ror", "mod",
         "rem", "abs", "not", "guarded", "reverse_range", "protected",
         "context", "`if", "`else", "`elsif", "`end", "`error", "`warning",
         "translate_off", "translate_on", "?=", "?/=", "?<", "?<=", "?>",
         "?>=", "register", "disconnect", "??", "<<", ">>", "force", "release",
         "parameter", "coverage on", "coverage off", "PSL directive", "always",
         "->", "<->", "default", "clock", "next!", "never", "eventually!",
         "next_a", "next_a!", "next_e", "next_e!", "next_event", "next_event!",
         "module", "endmodule", "input", "output", "reg", "posedge", "negedge",
         "initial", "wire", "unsigned", "assume", "assume_guarantee",
         "restrict", "restrict_guarantee", "strong", "fairness", "cover",
         "property", "sequence", "const", "mutable", "hdltype", "boolean",
         "bit", "bitvector", "numeric", "string", "[*", "[+]", "[=", "[->",
         "&&", "within", "system task", "view", "private", "prev", "stable",
         "rose", "fell", "ended", "nondet", "nondetv", "union", "translate on",
         "translate off", "until!", "until_", "until_!", "`timescale"
         "supply0", "supply1", "pulldown", "pullup", "===", "!==",
      };

      if (tok >= 200 && tok - 200 < ARRAY_LEN(token_strs))
         return token_strs[tok - 200];
   }

   return "???";
}

void free_token(token_t tok, yylval_t *lval)
{
   if (tok == tSTRING || tok == tBITSTRING)
      free(lval->str);

   DEBUG_ONLY(lval->str = NULL);
}

static void pp_defines_init(void)
{
   if (pp_defines != NULL)
      return;

   pp_defines = shash_new(16);

   pp_defines_add("VHDL_VERSION", standard_text(standard()));
   pp_defines_add("TOOL_TYPE",    "SIMULATION");
   pp_defines_add("TOOL_VENDOR",  PACKAGE_URL);
   pp_defines_add("TOOL_NAME",    PACKAGE_NAME);
   pp_defines_add("TOOL_EDITION", "GPL");
   pp_defines_add("TOOL_VERSION", PACKAGE_VERSION);
}

void pp_defines_add(const char *name, const char *value)
{
   pp_defines_init();

   char *existing_val = shash_get(pp_defines, name);
   if (existing_val) {
      warnf("conditional analysis identifier '%s' already defined (%s)",
            name, existing_val);
      free(existing_val);
   }

   shash_put(pp_defines, name, xstrdup(value));
}

const char *pp_defines_get(const char *name)
{
   pp_defines_init();
   return shash_get(pp_defines, name);
}

static int pp_yylex(void)
{
   const int tok = lookahead != -1 ? lookahead : yylex();
   lookahead = -1;
   return tok;
}

static void pp_error(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   if (pperrors++ == 0 || opt_get_int(OPT_UNIT_TEST)) {
      diag_t *d = diag_new(DIAG_ERROR, &yylloc);
      diag_vprintf(d, fmt, ap);
      diag_emit(d);
   }

   va_end(ap);
}

static bool pp_expect(int expect)
{
   const int got = pp_yylex();
   if (got != expect) {
      pp_error("expected $yellow$%s$$ while parsing conditional "
               "directive but found $yellow$%s$$", token_str(expect),
               token_str(got));
      return false;
   }

   return true;
}

static bool pp_cond_analysis_relation(void)
{
   // ( conditional_analysis_expression )
   //   | not ( conditional_analysis_expression )
   //   | conditional_analysis_identifier = string_literal
   //   | conditional_analysis_identifier /= string_literal
   //   | conditional_analysis_identifier < string_literal
   //   | conditional_analysis_identifier <= string_literal
   //   | conditional_analysis_identifier > string_literal
   //   | conditional_analysis_identifier >= string_literal

   bool result = false;
   token_t tok = pp_yylex();

   switch (tok) {
   case tLPAREN:
      result = pp_cond_analysis_expr();
      pp_expect(tRPAREN);
      break;

   case tNOT:
      pp_expect(tLPAREN);
      result = !pp_cond_analysis_expr();
      pp_expect(tRPAREN);
      break;

   case tID:
      {
         const char *name = istr(yylval.ident);
         token_t rel = pp_yylex();

         if (pp_expect(tSTRING)) {
            const char *value = shash_get(pp_defines, name);
            if (value == NULL)
               pp_error("undefined conditional analysis identifier %s", name);
            else {
               char *cmp = yylval.str + 1;
               cmp[strlen(cmp) - 1] = '\0';

               switch (rel) {
               case tEQ:
                  result = strcmp(value, cmp) == 0;
                  break;
               case tNEQ:
                  result = strcmp(value, cmp) != 0;
                  break;
               case tLT:
                  result = strcmp(value, cmp) < 0;
                  break;
               case tLE:
                  result = strcmp(value, cmp) <= 0;
                  break;
               case tGT:
                  result = strcmp(value, cmp) > 0;
                  break;
               case tGE:
                  result = strcmp(value, cmp) >= 0;
                  break;
               default:
                  pp_error("expected conditional analysis relation "
                           "but found $yellow$%s$$", token_str(rel));
                  break;
               }
            }

            free(yylval.str);
         }
      }
      break;

   default:
      pp_error("unexpected $yellow$%s$$ while parsing conditional "
               "analysis relation", token_str(tok));
   }

   return result;
}

static bool pp_cond_analysis_expr(void)
{
   // conditional_analysis_relation
   //   | conditional_analysis_relation { and conditional_analysis_relation }
   //   | conditional_analysis_relation { or conditional_analysis_relation }
   //   | conditional_analysis_relation { xor conditional_analysis_relation }
   //   | conditional_analysis_relation { xnor conditional_analysis_relation }

   const bool lhs = pp_cond_analysis_relation();
   switch ((lookahead = pp_yylex())) {
   case tAND:
      lookahead = -1;
      return pp_cond_analysis_relation() && lhs;
   case tOR:
      lookahead = -1;
      return pp_cond_analysis_relation() || lhs;
   case tXOR:
      lookahead = -1;
      return pp_cond_analysis_relation() ^ lhs;
   case tXNOR:
      lookahead = -1;
      return !(pp_cond_analysis_relation() ^ lhs);
   default:
      return lhs;
   }
   return lhs;
}

token_t processed_yylex(void)
{
   assert(lookahead == -1);

   for (;;) {
      token_t token = pp_yylex();
      switch (token) {
      case tCONDIF:
         {
            cond_state_t new = { .loc = yylloc };
            new.result = new.taken = pp_cond_analysis_expr();

            if (cond_stack.count > 0 && !ATOP(cond_stack).result) {
               // Suppress nested conditionals in not-taken branches
               new.taken = true;
               new.result = false;
            }

            pp_expect(tTHEN);

            new.loc.column_delta =
               yylloc.first_column + yylloc.column_delta - new.loc.first_column;
            new.loc.line_delta =
               yylloc.first_line + yylloc.line_delta - new.loc.first_line;

            APUSH(cond_stack, new);
         }
         break;

      case tCONDELSIF:
         {
            if (cond_stack.count == 0)
               pp_error("unexpected $yellow$%s$$ outside conditional "
                        "analysis block", token_str(token));

            const bool result = pp_cond_analysis_expr();

            if (cond_stack.count > 0) {
               if (!ATOP(cond_stack).taken) {
                  ATOP(cond_stack).result = result;
                  ATOP(cond_stack).taken = result;
               }
               else
                  ATOP(cond_stack).result = false;
            }

            pp_expect(tTHEN);
         }
         break;

      case tCONDELSE:
         {
            if (cond_stack.count == 0)
               pp_error("unexpected $yellow$%s$$ outside conditional "
                        "analysis block", token_str(token));
            else {
               cond_state_t *cs = &(cond_stack.items[cond_stack.count - 1]);
               cs->result = !(cs->taken);
            }
         }
         break;

      case tCONDEND:
         {
            if (cond_stack.count == 0)
               pp_error("unexpected $yellow$%s$$ outside conditional "
                        "analysis block", token_str(token));
            else
               APOP(cond_stack);

            if ((lookahead = yylex()) == tIF)
               lookahead = -1;
         }
         break;

      case tCONDERROR:
      case tCONDWARN:
         {
            loc_t loc = yylloc;
            if (pp_expect(tSTRING)) {
               loc.column_delta =
                  yylloc.first_column + yylloc.column_delta - loc.first_column;
               loc.line_delta =
                  yylloc.first_line + yylloc.line_delta - loc.first_line;

               if (cond_stack.count == 0 || ATOP(cond_stack).result) {
                  const diag_level_t level =
                     token == tCONDWARN ? DIAG_WARN : DIAG_ERROR;
                  diag_t *d = diag_new(level, &loc);
                  diag_printf(d, "%s", yylval.str);
                  diag_emit(d);
               }

               free(yylval.str);
            }
         }
         break;

      case tEOF:
         while (cond_stack.count > 0) {
            error_at(&(ATOP(cond_stack).loc), "unterminated conditional "
                     "analysis block");
            APOP(cond_stack);
         }
         return tEOF;

      default:
         if (cond_stack.count == 0 || ATOP(cond_stack).result)
            return token;
         else {
            free_token(token, &yylval);
            break;
         }
      }
   }
}
