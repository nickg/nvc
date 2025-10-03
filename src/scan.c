//
//  Copyright (C) 2014-2025  Nick Gasson
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
#include <errno.h>
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

typedef struct {
   ident_t     macro;
   char       *include;
   loc_t       expandloc;
   file_ref_t  file_ref;
} macro_expansion_t;

typedef A(macro_expansion_t) macro_stack_t;

typedef A(vlog_version_t) keywords_stack_t;

typedef A(char *) string_list_t;

typedef struct {
   const char *file_start;
   size_t      file_sz;
   const char *read_ptr;
   int         colno;
   int         lineno;
   int         lookahead;
   file_ref_t  file_ref;
} input_buf_t;

typedef A(input_buf_t) buf_stack_t;

static input_buf_t       input_buf;
static buf_stack_t       buf_stack;
static hdl_kind_t        src_kind;
static int               pperrors;
static cond_stack_t      cond_stack;
static shash_t          *pp_defines;
static macro_stack_t     macro_stack;
static vlog_version_t    default_keywords = VLOG_1800_2023;
static keywords_stack_t  keywords_stack;
static string_list_t     include_dirs;

extern int yylex(void);

extern void reset_scanner(void);

static bool pp_cond_analysis_expr(void);
static void pp_defines_init(void);

yylval_t yylval;
loc_t yylloc;

void input_from_buffer(const char *buf, size_t len, file_ref_t file_ref,
                       hdl_kind_t kind)
{
   pp_defines_init();

   reset_scanner();

   yylloc = LOC_INVALID;

   input_buf.file_start = buf;
   input_buf.file_sz    = len;
   input_buf.read_ptr   = buf;
   input_buf.lineno     = 1;
   input_buf.colno      = 0;
   input_buf.lookahead  = -1;
   input_buf.file_ref   = file_ref;

   src_kind = kind;
   pperrors = 0;

   switch (kind) {
   case SOURCE_VERILOG:
      reset_verilog_parser();
      break;
   case SOURCE_VHDL:
      reset_vhdl_parser();
      break;
   case SOURCE_SDF:
      reset_sdf_parser();
      break;
   }
}

void input_from_file(const char *file)
{
   // TODO: need a more sophisticated mechanism to determine HDL type
   hdl_kind_t kind = SOURCE_VHDL;
   size_t len = strlen(file);
   if (len > 2 && file[len - 2] == '.' && file[len - 1] == 'v') {
      kind = SOURCE_VERILOG;
      set_default_keywords(VLOG_1364_2001);
   }
   else if (len > 3 && strcmp(file + len - 3, ".sv") == 0) {
      kind = SOURCE_VERILOG;
      set_default_keywords(VLOG_1800_2023);
   }
   else if (len > 4 && !strcmp(&(file[len - 4]), ".sdf")) {
      kind = SOURCE_SDF;
   }

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
         if (bufsz - 1 - input_buf.file_sz == 0)
            buf = xrealloc(buf, bufsz *= 2);

         nbytes = read(fd, buf + total, bufsz - 1 - total);
         if (nbytes < 0)
            fatal_errno("read");

         total += nbytes;
         buf[total] = '\0';
      } while (nbytes > 0);

      input_from_buffer(buf, total, FILE_INVALID, kind);
   }
   else if (info.type == FILE_REGULAR) {
      void *map = NULL;
      if (info.size > 0)
         map = map_file(fd, info.size);

      file_ref_t file_ref = loc_file_ref(file, map);

      input_from_buffer(map, info.size, file_ref, kind);
   }
   else
      fatal("opening %s: not a regular file", file);

   close(fd);
}

void push_buffer(const char *buf, size_t len, file_ref_t file_ref)
{
   APUSH(buf_stack, input_buf);

   yylloc = LOC_INVALID;

   input_buf.file_start = buf;
   input_buf.file_sz    = len;
   input_buf.read_ptr   = buf;
   input_buf.lineno     = 1;
   input_buf.colno      = 0;
   input_buf.lookahead  = -1;
   input_buf.file_ref   = file_ref;
}

void push_file(const char *file, const loc_t *srcloc)
{
   int fd = open(file, O_RDONLY);
   if (fd < 0) {
      for (int i = 0; i < include_dirs.count; i++) {
         char path[PATH_MAX];
         checked_sprintf(path, sizeof(path), "%s" DIR_SEP "%s",
                         include_dirs.items[i], file);
         if ((fd = open(path, O_RDONLY)) != -1)
            break;
      }
   }

   if (fd < 0) {
      diag_t *d = diag_new(DIAG_ERROR, srcloc);
      if (errno != ENOENT)
         diag_printf(d, "opening %s: %s", file, last_os_error());
      else {
         char *cwd LOCAL = getcwd(NULL, 0);
         diag_printf(d, "cannot find %s in the current working directory %s",
                     file, cwd);
         if (include_dirs.count > 0) {
            diag_printf(d, " or any of the supplied include directories");
            for (int i = 0; i < include_dirs.count; i++)
               diag_hint(d, NULL, "searched include directory %s",
                         include_dirs.items[i]);
         }
         diag_hint(d, NULL, "add additional include directories with the "
                   "$bold$-I$$ option");
      }
      diag_emit(d);

      push_buffer("", 0, FILE_INVALID);
   }
   else {
      file_info_t info;
      if (!get_handle_info(fd, &info))
         fatal_errno("%s: cannot get file info", file);

      void *map = NULL;
      if (info.size > 0)
         map = map_file(fd, info.size);

      file_ref_t file_ref = loc_file_ref(file, map);

      push_buffer(map, info.size, file_ref);
   }
}

void pop_buffer(void)
{
   input_buf = APOP(buf_stack);
   yylloc = LOC_INVALID;
}

void add_include_dir(const char *str)
{
   APUSH(include_dirs, xstrdup(str));
}

hdl_kind_t source_kind(void)
{
   return src_kind;
}

int get_next_char(char *b, int max_buffer)
{
   const ptrdiff_t navail =
      (input_buf.file_start - input_buf.read_ptr) + input_buf.file_sz;
   assert(navail >= 0);

   if (navail == 0)
      return 0;

   const int nchars = MIN(navail, max_buffer);

   memcpy(b, input_buf.read_ptr, nchars);
   input_buf.read_ptr += nchars;

   return nchars;
}

void begin_token(char *tok, int length)
{
   // Newline must match as a single token for the logic below to work
   assert(strchr(tok, '\n') == NULL || length == 1);

   if (macro_stack.count == 0 || input_buf.file_ref != FILE_INVALID) {
      const int first_col = input_buf.colno;
      if (*tok == '\n') {
         input_buf.colno = 0;
         input_buf.lineno += 1;
      }
      else
         input_buf.colno += length;

      const int last_col = first_col + length - 1;

      extern loc_t yylloc;
      yylloc = get_loc(input_buf.lineno, first_col, input_buf.lineno,
                       last_col, input_buf.file_ref);
   }
   else {
      yylloc = LOC_INVALID;
      for (int i = 0; i < macro_stack.count && loc_invalid_p(&yylloc); i++)
         yylloc = macro_stack.items[i].expandloc;
   }
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
         "initial", "wire", "unsigned number", "assume", "assume_guarantee",
         "restrict", "restrict_guarantee", "strong", "fairness", "cover",
         "property", "sequence", "const", "mutable", "hdltype", "boolean",
         "bit", "bitvector", "numeric", "string", "[*", "[+]", "[->", "[=",
         "&&", "within", "system task", "view", "private", "prev", "stable",
         "rose", "fell", "ended", "nondet", "nondetv", "union", "translate on",
         "translate off", "until!", "until_", "until!_", "`timescale",
         "supply0", "supply1", "pulldown", "pullup", "===", "!==", "==", "!=",
         "(*", "*)", "number", "forever", "[[", "]]", "specify", "endspecify",
         "primitive", "endprimitive", "table", "endtable", "assign",
         "level symbol", "edge symbol", "edge indicator", "buf", "||",
         "scalar constant (0)", "scalar constant (1)", "delay file",
         "sdf version", "design", "date", "vendor", "program", "version",
         "divider", "voltage", "temperature", "cell", "celltype", "instance",
         "delay", "timing check", "timing env", "path pulse",
         "path pulse percent", "IO path", "retain", "cond", "condelse",
         "interconnect", "net delay", "device", "setup", "hold", "setuphold",
         "recovery", "removal", "recrem", "skew", "bidirectional skew", "width",
         "period", "nochange", "cond", "scond", "ccond", "path constraint",
         "period constraint", "sum", "diff", "skew constraint", "exception",
         "name", "arrival", "departure", "slack", "waveform", "increment",
         "absolute", "~&", "~|", "~^", "struct", "packed", "void", "byte",
         "shortint", "longint", "int", "integer", "time", "typedef", "logic",
         "enum", "tagged", "abort", "sync_abort", "async_abort", "before",
         "before!", "before_", "before!_", "|->", "|=>", "next", "inf",
         "repeat", "do", "endpoint", "<<", ">>", "<<<", ">>>", "task",
         "endtask", "endfunction", "`begin_keywords", "`end_keywords", "real",
         "shortreal", "realtime", "`__nvc_push", "`__nvc_pop", "++", "--",
         "var", "`default_nettype", "tri", "tri0", "tri1", "wand", "triand",
         "wor", "trior", "trireg", "uwire", "none", "localparam", "always_comb",
         "always_ff", "always_latch", "(*)", "endcase", "casex", "casez",
         "ifnone", "edge", "*>", "$setup", "$hold", "$recovery", "$removal",
         "$setuphold", "$recrem", "$width", "+:", "-:", "endgenerate",
         "`resetall", "event", "&&&", "specparam", "fork", "join", "automatic",
         "genvar", "highz0", "highz1", "strong0", "strong1", "pull0", "pull1",
         "weak0", "weak1", "small", "medium", "large", "vectored", "scalared",
         "`unconnected_drive", "`nounconnected_drive", "deassign", "signed",
         "unsigned", "disable", "class", "chandle", "export", "import",
         "endpackage", "extends", "this", "nettype", "ref", "super", "static",
         "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=", "<<<=",
         ">>>=", "bufif0", "bufif1", "notif0", "notif1", "`protect",
         "begin_protected", "end_protected", "endprogram", "endclass",
         "virtual", "::",
      };

      if (tok >= 200 && tok - 200 < ARRAY_LEN(token_strs))
         return token_strs[tok - 200];
   }

   return "???";
}

void free_token(token_t tok, yylval_t *lval)
{
   if (tok == tSTRING || tok == tBITSTRING || tok == tUNSNUM)
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

   bool valid = isalpha_iso88591(name[0]) || name[0] == '_';
   for (const char *p = name + 1; valid && *p; p++)
      valid &= isalnum_iso88591(*p) || *p == '_';

   if (!valid)
      errorf("\"%s\" is not a valid conditional analysis identifier", name);

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

void pp_defines_iter(pp_iter_t fn, void *ctx)
{
   pp_defines_init();

   const char *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN;
        shash_iter(pp_defines, &it, &key, &value); )
      (*fn)(key, value, ctx);
}

static int pp_yylex(void)
{
   const int tok = input_buf.lookahead != -1 ? input_buf.lookahead : yylex();
   input_buf.lookahead = -1;
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
   switch ((input_buf.lookahead = pp_yylex())) {
   case tAND:
      input_buf.lookahead = -1;
      return pp_cond_analysis_relation() && lhs;
   case tOR:
      input_buf.lookahead = -1;
      return pp_cond_analysis_relation() || lhs;
   case tXOR:
      input_buf.lookahead = -1;
      return pp_cond_analysis_relation() ^ lhs;
   case tXNOR:
      input_buf.lookahead = -1;
      return !(pp_cond_analysis_relation() ^ lhs);
   default:
      return lhs;
   }
   return lhs;
}

static void macro_hint_cb(diag_t *d, void *ctx)
{
   assert(macro_stack.count > 0);

   for (int i = 0; i < macro_stack.count; i++) {
      const macro_expansion_t *me = &(macro_stack.items[i]);
      if (me->include != NULL)
         diag_hint(d, &me->expandloc, "while processing `include \"%s\" "
                   "directive", me->include);
      else
         diag_hint(d, NULL, "while expanding macro %s", istr(me->macro));
   }
}

static void pp_nvc_push(void)
{
   token_t tok;
   macro_expansion_t exp = {};
   unsigned first_line, first_column, column_delta;

   switch ((tok = pp_yylex())) {
   case tID:
      exp.macro = yylval.ident;
      break;
   case tSTRING:
      exp.include = tb_claim(yylval.text);
      break;
   default:
      goto error;
   }

   if ((tok = pp_yylex()) != tCOMMA)
      goto error;

   if ((tok = pp_yylex()) != tUNSNUM)
      goto error;

   first_line = atoi(yylval.str);
   free(yylval.str);

   if ((tok = pp_yylex()) != tCOLON)
      goto error;

   if ((tok = pp_yylex()) != tUNSNUM)
      goto error;

   first_column = atoi(yylval.str);
   free(yylval.str);

   if ((tok = pp_yylex()) != tCOMMA)
      goto error;

   if ((tok = pp_yylex()) != tUNSNUM)
      goto error;

   column_delta = atoi(yylval.str);
   free(yylval.str);

   exp.expandloc = get_loc(first_line, first_column, first_line,
                           first_column + column_delta, yylloc.file_ref);

   input_buf.lineno = 0;
   input_buf.colno = 0;

   if (exp.include != NULL)
      input_buf.file_ref = loc_file_ref(exp.include, NULL);
   else
      input_buf.file_ref = FILE_INVALID;

   yylloc = LOC_INVALID;

   if (macro_stack.count == 0)
      diag_add_hint_fn(macro_hint_cb, NULL);

   APUSH(macro_stack, exp);
   return;

 error:
   pp_error("unexpected %s while parsing `__nvc_push directive",
            token_str(tok));
}

static void pp_nvc_pop(void)
{
   if (macro_stack.count == 0)
      return;

   const macro_expansion_t top = APOP(macro_stack);

   input_buf.lineno = top.expandloc.first_line - 1;
   input_buf.file_ref = top.expandloc.file_ref;

   // Eat the following newline and adjust the next token location
   input_buf.lookahead = yylex();

   yylloc.first_column +=
      top.expandloc.first_column + top.expandloc.column_delta + 1;

   input_buf.colno = yylloc.first_column + yylloc.column_delta;

   free(top.include);

   if (macro_stack.count == 0)
      diag_remove_hint_fn(macro_hint_cb);
}

static void pp_protect_block(void)
{
   if (!pp_expect(tBEGINPROT))
      return;

   diag_t *d = diag_new(DIAG_ERROR, &yylloc);
   diag_printf(d, "`protect directives are not supported");
   diag_hint(d, NULL, "encrypted IP can only be practicably implemented by "
             "proprietary tools as the decryption key is shared between "
             "the simulator and IP vendors");
   diag_lrm(d, STD_08, "24.1");
   diag_emit(d);

   for (;;) {
      token_t token = pp_yylex();
      switch (token) {
      case tEOF:
         pp_error("unexpected end-of-file before $yellow$end_protected$$");
         return;
      case tPROTECT:
         if (pp_yylex() == tENDPROT)
            return;
         break;
      }
   }
}

token_t processed_yylex(void)
{
   assert(input_buf.lookahead == -1);

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

            if ((input_buf.lookahead = yylex()) == tIF)
               input_buf.lookahead = -1;
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

      case tNVCPUSH:
         pp_nvc_push();
         break;

      case tNVCPOP:
         pp_nvc_pop();
         break;

      case tPROTECT:
         pp_protect_block();
         break;

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

const char *verilog_version_string(vlog_version_t vers)
{
   static const char *const map[] = {
      [VLOG_1364_1995] = "1364-1995",
      [VLOG_1364_2001_NOCONFIG] = "1364-2001-noconfig",
      [VLOG_1364_2001] = "1364-2001",
      [VLOG_1364_2005] = "1364-2005",
      [VLOG_1800_2005] = "1800-2005",
      [VLOG_1800_2009] = "1800-2009",
      [VLOG_1800_2012] = "1800-2012",
      [VLOG_1800_2017] = "1800-2017",
      [VLOG_1800_2023] = "1800-2023",
   };
   return map[vers];
}

bool parse_verilog_version(const char *str, vlog_version_t *vers)
{
   for (vlog_version_t i = 0; i < VLOG_1800_2023 + 1; i++) {
      if (strcmp(str, verilog_version_string(i)) == 0) {
         *vers = i;
         return true;
      }
   }

   return false;
}

void set_default_keywords(vlog_version_t vers)
{
   default_keywords = vers;
}

void push_keywords(vlog_version_t vers)
{
   APUSH(keywords_stack, vers);
}

bool pop_keywords(void)
{
   if (keywords_stack.count == 0)
      return false;
   else {
      APOP(keywords_stack);
      return true;
   }
}

bool get_verilog_keywords(vlog_version_t *vers)
{
   if (keywords_stack.count == 0) {
      *vers = default_keywords;
      return false;
   }
   else {
      *vers = keywords_stack.items[keywords_stack.count - 1];
      return true;
   }
}
