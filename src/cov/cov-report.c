//
//  Copyright (C) 2013-2023  Nick Gasson
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
#include "cov/cov-api.h"
#include "cov/cov-data.h"
#include "hash.h"
#include "ident.h"
#include "lib.h"
#include "option.h"
#include "thirdparty/sha1.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <libgen.h>
#include <inttypes.h>

#define MARGIN_LEFT "20%%"
#define SIDEBAR_WIDTH "15%%"

struct _cover_rpt_buf {
   text_buf_t      *tb;
   cover_rpt_buf_t *prev;
};

typedef struct {
   unsigned    total_stmts;
   unsigned    hit_stmts;
   unsigned    total_branches;
   unsigned    hit_branches;
   unsigned    total_toggles;
   unsigned    hit_toggles;
   unsigned    total_expressions;
   unsigned    hit_expressions;
   unsigned    total_states;
   unsigned    hit_states;
   unsigned    total_functional;
   unsigned    hit_functional;
} cover_stats_t;

typedef struct {
   char       *text;
   size_t      len;
} cover_line_t;

typedef struct {
   cover_line_t *line;
   cover_item_t *item;
} cover_pair_t;

typedef struct {
   cover_pair_t *hits;
   cover_pair_t *miss;
   cover_pair_t *excl;
   int           n_hits;
   int           n_miss;
   int           n_excl;
   int           alloc_hits;
   int           alloc_miss;
   int           alloc_excl;
} cover_chain_t;

typedef struct {
   const char   *name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
} cover_file_t;

typedef struct {
   cover_chain_t   stmt;
   cover_chain_t   branch;
   cover_chain_t   toggle;
   cover_chain_t   expression;
   cover_chain_t   state;
   cover_chain_t   functional;
} cover_chain_group_t;

typedef A(cover_item_t *) item_ptr_array_t;

struct _cover_rpt_file_ctx {
   cover_file_t         *file;
   item_ptr_array_t      items;
   cover_stats_t         stats;
   cover_chain_group_t   chns;
} ;

struct _cover_rpt_hier_ctx {
   cover_data_t           *data;
   cover_stats_t           flat_stats;
   cover_stats_t           nested_stats;
   cover_rpt_hier_ctx_t   *parent;
   cover_chain_group_t     chns;
   int                     lvl;
};

typedef enum {
   PAIR_UNCOVERED    = 0,
   PAIR_EXCLUDED     = 1,
   PAIR_COVERED      = 2,
   PAIR_LAST         = 3
} cov_pair_kind_t;

#define INIT_CHAIN(ctx, name)                                           \
   ctx->chns.name.hits = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->chns.name.miss = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->chns.name.excl = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->chns.name.alloc_hits = 1024;                                         \
   ctx->chns.name.alloc_miss = 1024;                                         \
   ctx->chns.name.alloc_excl = 1024;                                         \

#define COV_RPT_TITLE "NVC code coverage report"

static void cover_report_hier_children(cover_rpt_hier_ctx_t *ctx,
                                  cover_scope_t *s, const char *dir,
                                  FILE *summf, int *skipped);

static shash_t *cover_files = NULL;


///////////////////////////////////////////////////////////////////////////////
// Common reporting functions
///////////////////////////////////////////////////////////////////////////////

static void cover_append_line(cover_file_t *f, const char *buf)
{
   if (f->n_lines == f->alloc_lines) {
      f->alloc_lines *= 2;
      f->lines = xrealloc_array(f->lines, f->alloc_lines, sizeof(cover_line_t));
   }

   cover_line_t *l = &(f->lines[(f->n_lines)++]);
   l->text = xstrdup(buf);
   l->len  = strlen(buf);
}

static cover_file_t *cover_file_for_scope(cover_scope_t *s)
{
   if (loc_invalid_p(&(s->loc)))
      return NULL;

   if (cover_files == NULL)
      cover_files = shash_new(64);

   const char *name = loc_file_str(&(s->loc));
   cover_file_t *f = shash_get(cover_files, name);

   if (f != NULL)
      return f->valid ? f : NULL;

   f = xmalloc(sizeof(cover_file_t));
   f->name        = name;
   f->n_lines     = 0;
   f->alloc_lines = 1024;
   f->lines       = xmalloc_array(f->alloc_lines, sizeof(cover_line_t));
   f->valid       = false;

   shash_put(cover_files, name, f);

   FILE *fp = fopen(name, "r");

   if (fp == NULL) {
      // Guess the path is relative to the work library
      char *path LOCAL = xasprintf("%s/../%s", lib_path(lib_work()), name);
      fp = fopen(path, "r");
   }

   if (fp == NULL) {
      warn_at(&(s->loc), "omitting hierarchy %s from the coverage report as "
              "the correpsonding source file could not be found",
              istr(s->hier));
      return NULL;
   }

   f->valid = true;

   while (!feof(fp)) {
      char buf[1024];
      if (fgets(buf, sizeof(buf), fp) != NULL)
         cover_append_line(f, buf);
      else if (ferror(fp))
         fatal("error reading %s", name);
   }

   fclose(fp);

   return f;
}

static void cover_print_html_header(FILE *f)
{
   fprintf(f, "<!DOCTYPE html>\n"
              "<html lang=\"en\">\n"
              "  <head>\n"
              "  <title>");

   fprintf(f, COV_RPT_TITLE "\n");

   fprintf(f, "</title>\n"
              "  <style>\n"
              "   header {\n"
              "      padding: 30px;\n"
              "      text-align: center;\n"
              "      font-size: 35px;\n"
              "   }\n"
              "   h2 {\n"
              "      word-wrap: break-word;\n"
              "      width:75%%\n"
              "   }\n"
              "   h3 {\n"
              "      word-wrap: break-word;\n"
              "      margin-bottom: 0px;\n"
              "   }\n"
              "   hr {\n"
              "      border:none;\n"
              "      height: 2px;\n"
              "      background: black;\n"
              "   }\n"
              "   footer{\n"
              "      display: table;\n"
              "      text-align: center;\n"
              "      margin-left: auto;\n"
              "      margin-right: auto;\n"
              "   }\n"
              "   nav {\n"
              "      float: left;\n"
              "      background-color: #ccc;\n"
              "      width: " SIDEBAR_WIDTH ";\n"
              "      overflow: auto; \n"
              "      padding: 10px;\n"
              "      margin-top: 100px;\n"
              "     }\n"
              "   table {\n"
              "     table-layout: fixed;"
              "   }\n"
              "   table, th, td {\n"
              "     border: 2px solid black;\n"
              "     border-collapse: collapse;\n"
              "     word-wrap: break-word;\n"
              "   }\n"
              "   .tabcontent {\n"
              "     display: none;\n"
              "     padding: 0px 0px;\n"
              "     border: 2px solid #ccc;\n"
              "     border-top: none;\n"
              "     word-wrap: break-word;\n"
              "   }\n"
              "   .tab {\n"
              "     overflow: hidden;\n"
              "     border: none;\n"
              "     margin-left: " MARGIN_LEFT ";\n"
              "     margin-top: 10px;\n"
              "   }\n"
              "   .tab button.active {\n"
              "     background-color: #ccc;\n"
              "   }\n"
              "   .tab button:hover {\n"
              "     background-color: #ddd;\n"
              "   }\n"
              "   .tab button {\n"
              "     background-color: inherit;\n"
              "     float: left;\n"
              "     margin-left: 20px;\n"
              "     border: 2px solid black;\n"
              "     cursor: pointer;\n"
              "     padding: 14px 16px;\n"
              "     font-size: 17px;\n"
              "   }\n"
              "   .cbg:hover {\n"
              "     background-color: #dddddd;\n"
              "   }\n"
              "   .cbg {\n"
              "     background-color: #bbbbbb;\n"
              "   }\n"
              "   .cbt {\n"
              "     margin-top: 8px;\n"
              "   }\n"
              "   .cbt th {\n"
              "     background-color: #bbbbbb;\n"
              "     text-align: center;\n"
              "    }\n"
              "   .cbt td, .cbt th {\n"
              "     width:50px;\n"
              "     text-align: center;\n"
              "    }\n"
              "   .cbt td + td, .cbt th + th { width:150px; }\n"
              "   .cbt td + td + td, .cbt th + th + th { width:150px; }\n"
              "   .cbt td + td + td + td, .cbt th + th + th + th { width:150px; }\n"
              "   .percent100 { background-color: #00cc00; }\n"
              "   .percent90 { background-color: #e6e600; }\n"
              "   .percent80 { background-color: #ff9900; }\n"
              "   .percent0 { background-color: #ff0000; }\n"
              "   .percentna { background-color: #aaaaaa; }\n"
              "  </style>\n"
              "</head>\n"
              "<body>\n\n");

   fprintf(f, "<header>");
   fprintf(f, COV_RPT_TITLE "\n");
   fprintf(f, "</header>\n\n");
}

static void cover_print_file_name(FILE *f, const char *name)
{
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   fprintf(f, "   File:&nbsp; <a href=\"../../%s\">../../%s</a>\n",
            name, name);
   fprintf(f, "</h2>\n\n");
}

static void cover_print_inst_name(FILE *f, cover_scope_t *s)
{
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   fprintf(f, "   Instance:&nbsp;%s\n", istr(s->hier));
   fprintf(f, "</h2>\n\n");
}

static void cover_print_percents_cell(FILE *f, unsigned hit, unsigned total)
{
   if (total > 0) {
      float perc = ((float) hit / (float) total) * 100;
      const char *class = "percent0";
      if (hit == total)
         class = "percent100";
      else if (perc > 90)
         class = "percent90";
      else if (perc > 80)
         class = "percent80";
      else
         class = "percent0";

      fprintf(f, "    <td class=\"%s\">%.1f %% (%d/%d)</td>\n",
              class, perc, hit, total);
   }
   else
      fprintf(f, "    <td class=\"percentna\">N.A.</td>\n");
}

static void cover_print_summary_table_header(FILE *f, const char *table_id,
                                             bool is_instance)
{
   const char *first_col_str = (is_instance) ? "Instance" : "File";

   fprintf(f, "<table id=\"%s\" style=\"width:75%%;margin-left:" MARGIN_LEFT ";margin-right:auto;\"> \n"
              "  <tr style=\"height:40px\">\n"
              "    <th class=\"cbg\" onclick=\"sortTable(0, &quot;%s&quot;)\" style=\"width:30%%\">%s</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(1, &quot;%s&quot;)\"  style=\"width:8%%\">Statement</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(2, &quot;%s&quot;)\"  style=\"width:8%%\">Branch</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(3, &quot;%s&quot;)\"  style=\"width:8%%\">Toggle</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(4, &quot;%s&quot;)\"  style=\"width:8%%\">Expression</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(5, &quot;%s&quot;)\"  style=\"width:8%%\">FSM state</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(6, &quot;%s&quot;)\"  style=\"width:8%%\">Functional</th>\n"
              "    <th class=\"cbg\" onclick=\"sortTable(7, &quot;%s&quot;)\"  style=\"width:8%%\">Average</th>\n"
              "  </tr>\n", table_id, table_id, first_col_str, table_id, table_id, table_id, table_id,
                           table_id, table_id, table_id);
}

static void cover_print_table_footer(FILE *f)
{
   fprintf(f, "</table>\n\n");
}

static void cover_print_timestamp(FILE *f)
{
   time_t t;
   time(&t);

   fprintf(f, "<footer>");
   fprintf(f, "   <p> NVC version: %s </p>\n", PACKAGE_VERSION);
   fprintf(f, "   <p> Generated on: %s </p>\n", ctime(&t));
   fprintf(f, "</footer>\n");

   fprintf(f, "</body>\n");
   fprintf(f, "</html>\n");
}

static inline void cover_print_char(FILE *f, char c)
{
   switch (c) {
   case '\n':
   case ' ': fputs("&nbsp;", f); break;
   case '<': fputs("&lt;", f); break;
   case '>': fputs("&gt;", f); break;
   case '&': fputs("&amp;", f); break;
   case '\t':   // TODO: handle tabs better
   default: fputc(c, f); break;
   }
}

static void cover_print_single_code_line(FILE *f, loc_t loc, cover_line_t *line)
{
   assert(loc.line_delta == 0);

   if (line->text == NULL)
      return;

   const size_t len = strlen(line->text);
   for (int curr_char = 0; curr_char < len; curr_char++) {

      // Highlight code location
      if (curr_char == loc.first_column)
         fprintf(f, "<code class=\"cbg\">");

      cover_print_char(f, line->text[curr_char]);

      // Finish code highlight
      if (curr_char == (loc.first_column + loc.column_delta))
         fprintf(f, "</code>");
   }
}

static void cover_print_lhs_rhs_arrows(FILE *f, cover_pair_t *pair)
{
   int last = strlen(pair->line->text);
   int curr = 0;

   // Offset by line number width
   int digits = pair->item->loc.first_line;
   do {
      digits /= 10;
      fprintf(f, "&nbsp");
   } while (digits > 0);
   fprintf(f, "&nbsp");

   loc_t *loc_lhs = &(pair->item->loc_lhs);
   loc_t *loc_rhs = &(pair->item->loc_rhs);

   int lhs_beg = loc_lhs->first_column;
   int rhs_beg = loc_rhs->first_column;

   int lhs_end = lhs_beg + loc_lhs->column_delta;
   int rhs_end = rhs_beg + loc_rhs->column_delta;

   int lhs_mid = (lhs_end + lhs_beg) / 2;
   int rhs_mid = (rhs_end + rhs_beg) / 2;

   while (curr < last) {
      if (curr == lhs_mid - 1)
         fprintf(f, "L");
      else if (curr == lhs_mid)
         fprintf(f, "H");
      else if (curr == lhs_mid + 1)
         fprintf(f, "S");

      else if (curr == lhs_beg)
         fprintf(f, "<");
      else if (curr > lhs_beg && curr < lhs_end)
         fprintf(f, "-");
      else if (curr == (lhs_beg + loc_lhs->column_delta))
         fprintf(f, ">");

      else if (curr == rhs_mid - 1)
         fprintf(f, "R");
      else if (curr == rhs_mid)
         fprintf(f, "H");
      else if (curr == rhs_mid + 1)
         fprintf(f, "S");

      else if (curr == rhs_beg)
         fprintf(f, "<");
      else if (curr > rhs_beg && curr < rhs_end)
         fprintf(f, "-");
      else if (curr == (rhs_beg + loc_rhs->column_delta))
         fprintf(f, ">");

      else
         fprintf(f, "&nbsp");

      curr++;
   }
}

static void cover_print_item_title(FILE *f, cover_pair_t *pair)
{
   static const char *text[] = {
      [COV_SRC_IF_CONDITION] = "\"if\" / \"when\" / \"else\" condition",
      [COV_SRC_CASE_CHOICE] = "\"case\" / \"with\" / \"select\" choice",
      [COV_SRC_LOOP_CONTROL] = "Loop control condition",
      [COV_SRC_ASSERT] = "Assertion statement",
      [COV_SRC_REPORT] = "Report statement",
      [COV_SRC_IF_STMT] = "If statement",
      [COV_SRC_SIGNAL_ASSIGN] = "Signal assignment statement",
      [COV_SRC_VAR_ASSIGN] = "Variable assignment statement",
      [COV_SRC_WAIT] = "Wait statement",
      [COV_SRC_LOOP_STMT] = "Loop statement",
      [COV_SRC_STATEMENT] = "Sequential statement",
      [COV_SRC_CONDITION] = "Condition",
      [COV_SRC_PSL_COVER] = "PSL cover point",
      [COV_SRC_USER_COVER] = "User cover point",
      [COV_SRC_UNKNOWN] = "",
   };

   fprintf(f, "<h3>");

   switch (pair->item->kind) {
   case COV_ITEM_STMT:
   case COV_ITEM_BRANCH:
   case COV_ITEM_FUNCTIONAL:
   {
      fprintf(f, "%s:", text[pair->item->source]);
      break;
   }
   case COV_ITEM_EXPRESSION:
      fprintf(f, "%s expression", istr(pair->item->func_name));
      break;
   case COV_ITEM_STATE:
      fprintf(f, "\"%s\" FSM", istr(pair->item->func_name));
      break;
   default:
      break;
   }
   fprintf(f, "</h3>");
}

static void cover_print_code_loc(FILE *f, cover_pair_t *pair)
{
   loc_t loc = pair->item->loc;
   cover_line_t *curr_line = pair->line;
   cover_line_t *last_line = pair->line + loc.line_delta;

   if (loc.line_delta == 0) {
      fprintf(f, "<code>");
      fprintf(f, "%d:", loc.first_line);

      cover_print_single_code_line(f, loc, curr_line);
      if (pair->item->flags & COVER_FLAGS_LHS_RHS_BINS) {
         fprintf(f, "<br>");
         cover_print_lhs_rhs_arrows(f, pair);
      }
      fprintf(f, "</code>");
   }
   else {
      fprintf(f, "<code>");

      do {
         // Shorten code samples longer than 5 lines
         if (loc.line_delta > 5 &&
             curr_line == pair->line + 2) {
            fprintf(f, "...<br>");
            curr_line = last_line - 1;
            continue;
         }
         else
            fprintf(f, "%zu:", loc.first_line + (curr_line - pair->line));

         int curr_char = 0;
         while (curr_char < curr_line->len) {
            cover_print_char(f, curr_line->text[curr_char]);
            curr_char++;
         }

         if (curr_line < last_line)
            fprintf(f, "<br>");
         curr_line++;

      } while (curr_line <= last_line);

      fprintf(f, "</code>");
   }
}

static void cover_print_get_exclude_button(FILE *f, cover_item_t *item,
                                           uint32_t flag, bool add_td)
{
   if (add_td)
      fprintf(f, "<td>");

   bool out_of_table = false;
   if (item->kind == COV_ITEM_STMT)
      out_of_table = true;
   else if ((item->kind == COV_ITEM_FUNCTIONAL) &&
            ((item->flags & COV_FLAG_USER_DEFINED) == 0))
      out_of_table = true;

   fprintf(f, "<button onclick=\"GetExclude('exclude %s')\" %s>"
           "Copy %sto Clipboard</button>", istr(item->hier),
           out_of_table ? "style=\"float: right;\"" : "",
           out_of_table ? "Exclude Command " : "");

   if (add_td)
      fprintf(f, "</td>");
}

static void cover_print_bin(FILE *f, cover_pair_t *pair, uint32_t flag,
                            cov_pair_kind_t pkind, int cols, const char **vals)
{
   if (pair->item->flags & flag) {
      fprintf(f, "<tr><td><b>Bin</b></td>");

      for (int i = 0; i < cols; i++)
         fprintf(f, "<td>%s</td>", vals[i]);

      // Toggle flags hold unreachability in highest bit of runtime data
      // Must be masked out to print properly
      fprintf(f, "<td>%d</td>", (pair->item->data) & ~COV_FLAG_UNREACHABLE);
      fprintf(f, "<td>%d</td>", pair->item->atleast);

      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(f, pair->item, flag, true);

      if (pkind == PAIR_EXCLUDED) {
         cover_flags_t flags = pair->item->flags;
         const char *er = (flags & COV_FLAG_UNREACHABLE)   ? "Unreachable" :
                          (flags & COV_FLAG_EXCLUDED_USER) ? "User exclude" :
                                                             "Exclude file";
         fprintf(f, "<td>%s</td>", er);
      }

      fprintf(f, "</tr>");
   }
}

static void cover_print_bin_header(FILE *f, cov_pair_kind_t pkind, int cols,
                                   const char **titles)
{
   fprintf(f, "<br><table class=\"cbt\">");
   fprintf(f, "<tr><th></th>");

   for (int i = 0; i < cols; i++) {
      const char *val = titles[i];
      fprintf(f, "<th>%s</th>", val);
   }

   fprintf(f, "<th>Count</th>");
   fprintf(f, "<th>Threshold</th>");

   if (pkind == PAIR_UNCOVERED)
      fprintf(f, "<th>Exclude Command</th>");

   if (pkind == PAIR_EXCLUDED)
      fprintf(f, "<th>Excluded due to</th>");

   fprintf(f, "</tr>");
}

static void cover_print_bins(FILE *f, cover_pair_t *first_pair, cov_pair_kind_t pkind)
{
   cover_pair_t *last_pair = first_pair + first_pair->item->consecutive - 1;
   for (cover_pair_t *pair = first_pair; pair <= last_pair; pair++)
   {
      loc_t loc = pair->item->loc;

      switch (pair->item->kind) {
      case COV_ITEM_BRANCH:
      {
         const char *v_true = "True";
         const char *v_false = "False";

         cover_print_bin(f, pair, COV_FLAG_TRUE, pkind, 1, &v_true);
         cover_print_bin(f, pair, COV_FLAG_FALSE, pkind, 1, &v_false);

         if (pair->item->flags & COV_FLAG_CHOICE) {
            int curr = loc.first_column;
            int last = (loc.line_delta) ? strlen(pair->line->text) :
                                          loc.column_delta + curr;

            LOCAL_TEXT_BUF tb = tb_new();
            tb_printf(tb, "<code>");
            while (curr <= last) {
               tb_printf(tb, "%c", pair->line->text[curr]);
               curr++;
            }
            tb_printf(tb, "</code>");

            const char *v = tb_get(tb);
            cover_print_bin(f, pair, COV_FLAG_CHOICE, pkind, 1, &v);
         }
         break;
      }

      case COV_ITEM_TOGGLE:
      {
         const char *v_01[2] = {"0", "1"};
         const char *v_10[2] = {"1", "0"};

         cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_1, pkind, 2, v_01);
         cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_0, pkind, 2, v_10);
         break;
      }

      case COV_ITEM_STATE:
      {
         ident_t state_name = ident_rfrom(pair->item->hier, '.');
         const char *v = istr(state_name);
         cover_print_bin(f, pair, COV_FLAG_STATE, pkind, 1, &v);
         break;
      }

      case COV_ITEM_EXPRESSION:
      {
         const char *t_str = (pair->item->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'1'" : "True";
         const char *f_str = (pair->item->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'0'" : "False";

         const char *ff[2] = {f_str, f_str};
         const char *ft[2] = {f_str, t_str};
         const char *tf[2] = {t_str, f_str};
         const char *tt[2] = {t_str, t_str};

         if ((pair->item->flags & COV_FLAG_TRUE) || (pair->item->flags & COV_FLAG_FALSE)) {
            cover_print_bin(f, pair, COV_FLAG_TRUE, pkind, 1, &t_str);
            cover_print_bin(f, pair, COV_FLAG_FALSE, pkind, 1, &f_str);
         }
         else if (pair->item->flags & COV_FLAG_00 || pair->item->flags & COV_FLAG_01 ||
                  pair->item->flags & COV_FLAG_10 || pair->item->flags & COV_FLAG_11) {
            cover_print_bin(f, pair, COV_FLAG_00, pkind, 2, ff);
            cover_print_bin(f, pair, COV_FLAG_01, pkind, 2, ft);
            cover_print_bin(f, pair, COV_FLAG_10, pkind, 2, tf);
            cover_print_bin(f, pair, COV_FLAG_11, pkind, 2, tt);
         }
         break;
      }

      case COV_ITEM_FUNCTIONAL:
      {
         cover_item_t *item = pair->item;
         assert (item->source == COV_SRC_USER_COVER);

         const char *v[item->n_ranges] LOCAL;
         for (int i = 0; i < item->n_ranges; i++)
            if (item->ranges[i].min == item->ranges[i].max)
               v[i] = xasprintf("%"PRIi64, item->ranges[i].min);
            else
               v[i] = xasprintf("%"PRIi64" - %"PRIi64, item->ranges[i].min,
                                item->ranges[i].max);

         cover_print_bin(f, pair, COV_FLAG_USER_DEFINED, pkind, item->n_ranges, v);
         break;
      }

      default:
         fatal("unsupported type of code coverage: %d at 'cover_print_bins' !", pair->item->kind);
      }
   }

   fprintf(f, "</table>");
}

static void cover_print_pairs(FILE *f, cover_pair_t *first, cov_pair_kind_t pkind,
                              int pair_cnt)
{
   if (pair_cnt == 0)
      return;

   cover_pair_t *last = first + pair_cnt - 1;
   cover_pair_t *curr = first;
   int step;

   do {
      if (curr > first) fprintf(f, "<hr>");

      step = curr->item->consecutive;

      switch (curr->item->kind) {
      case COV_ITEM_STMT:
         if (pkind == PAIR_UNCOVERED)
            cover_print_get_exclude_button(f, curr->item, 0, false);
         if (pkind == PAIR_EXCLUDED)
            fprintf(f, "<div style=\"float: right\"><b>Excluded due to:</b> Exclude file</div>");

         cover_print_item_title(f, curr);
         cover_print_code_loc(f, curr);
         fprintf(f, "<br><b>Count:</b> %d", curr->item->data);
         fprintf(f, "<br><b>Threshold:</b> %d", curr->item->atleast);
         break;

      case COV_ITEM_BRANCH:
      {
         cover_print_item_title(f, curr);
         cover_print_code_loc(f, curr);

         const char *title = (curr->item->flags & COV_FLAG_CHOICE) ? "Choice of" : "Evaluated to";
         cover_print_bin_header(f, pkind, 1, &title);

         cover_print_bins(f, curr, pkind);
         break;
      }

      case COV_ITEM_TOGGLE:
      {
         if (curr->item->flags & COV_FLAG_TOGGLE_SIGNAL)
            fprintf(f, "<h3>Signal:</h3>");
         else if (curr->item->flags & COV_FLAG_TOGGLE_PORT)
            fprintf(f, "<h3>Port:</h3>");

         const char *sig_name = istr(ident_runtil(curr->item->hier, '.'));
         sig_name += curr->item->metadata;
         fprintf(f, "&nbsp;<code>%s</code>", sig_name);

         const char *title[2] = {"From", "To"};
         cover_print_bin_header(f, pkind, 2, title);

         cover_print_bins(f, curr, pkind);
         break;
      }

      case COV_ITEM_EXPRESSION:
      {
         cover_print_item_title(f, curr);
         cover_print_code_loc(f, curr);

         if ((curr->item->flags & COV_FLAG_TRUE) || (curr->item->flags & COV_FLAG_FALSE)) {
            const char *title = "Evaluated to";
            cover_print_bin_header(f, pkind, 1, &title);
         }
         else {
            const char *title[2] = {"LHS", "RHS"};
            cover_print_bin_header(f, pkind, 2, title);
         }

         cover_print_bins(f, curr, pkind);
         break;
      }

      case COV_ITEM_STATE:
         cover_print_item_title(f, curr);
         cover_print_code_loc(f, curr);

         const char *title = "State";
         cover_print_bin_header(f, pkind, 1, &title);

         cover_print_bins(f, curr, pkind);
         break;

      case COV_ITEM_FUNCTIONAL:
         if (curr->item->source == COV_SRC_USER_COVER) {
            cover_print_item_title(f, curr);
            fprintf(f, "<br>%s", istr(curr->item->func_name));

            const char *title[curr->item->n_ranges] LOCAL;

            for (int i = 0; i < curr->item->n_ranges; i++)
               title[i] = xasprintf("Variable %d", i);

            cover_print_bin_header(f, pkind, curr->item->n_ranges, title);
            cover_print_bins(f, curr, pkind);
         }
         else {
            if (pkind == PAIR_UNCOVERED)
               cover_print_get_exclude_button(f, curr->item, 0, false);
            cover_print_item_title(f, curr);
            cover_print_code_loc(f, curr);
            fprintf(f, "<br><b>Count:</b> %d", curr->item->data);
            fprintf(f, "<br><b>Threshold:</b> %d", curr->item->atleast);
         }
         break;

      default:
         fatal("unsupported type of code coverage: %d at 'cover_print_pairs' !",
               curr->item->kind);
      }

      curr += step;

   } while (curr <= last);

   fprintf(f, "<div style=\"height:10px\"></div>\n");
}

static void cover_print_chain(FILE *f, cover_data_t *data, cover_chain_t *chn,
                              cover_item_kind_t kind)
{
   // HTML TAB
   fprintf(f, "<div id=\"");
   if (kind == COV_ITEM_STMT)
      fprintf(f, "Statement");
   else if (kind == COV_ITEM_BRANCH)
      fprintf(f, "Branch");
   else if (kind == COV_ITEM_TOGGLE)
      fprintf(f, "Toggle");
   else if (kind == COV_ITEM_EXPRESSION)
      fprintf(f, "Expression");
   else if (kind == COV_ITEM_STATE)
      fprintf(f, "FSM_state");
   else if (kind == COV_ITEM_FUNCTIONAL)
      fprintf(f, "Functional");

   fprintf(f, "\" class=\"tabcontent\" style=\"width:68.5%%;margin-left:" MARGIN_LEFT "; "
                          "margin-right:auto; margin-top:10px; border: 2px solid black;\">\n");

   for (cov_pair_kind_t pkind = PAIR_UNCOVERED; pkind < PAIR_LAST; pkind++) {
      int n;
      cover_pair_t *first_pair;

      if (pkind == PAIR_UNCOVERED) {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_UNCOVERED))
            continue;
         first_pair = chn->miss;
         n = chn->n_miss;
      }
      else if (pkind == PAIR_EXCLUDED) {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_EXCLUDED))
            continue;
         first_pair = chn->excl;
         n = chn->n_excl;
      }
      else {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_COVERED))
            continue;
         first_pair = chn->hits;
         n = chn->n_hits;
      }

      fprintf(f, "  <section style=\"background-color:");
      if (pkind == PAIR_UNCOVERED)
         fprintf(f, "#ffcccc;\">\n");
      else if (pkind == PAIR_EXCLUDED)
         fprintf(f, "#d6eaf8;\">\n");
      else
         fprintf(f, "#ccffcc;\">\n");

      fprintf(f, " <h2>");
      if (pkind == PAIR_UNCOVERED)
         fprintf(f, "Uncovered ");
      else if (pkind == PAIR_EXCLUDED)
         fprintf(f, "Excluded ");
      else
         fprintf(f, "Covered ");

      if (kind == COV_ITEM_STMT)
         fprintf(f, "statements:");
      else if (kind == COV_ITEM_BRANCH)
         fprintf(f, "branches:");
      else if (kind == COV_ITEM_TOGGLE)
         fprintf(f, "toggles:");
      else if (kind == COV_ITEM_EXPRESSION)
         fprintf(f, "expressions:");
      else if (kind == COV_ITEM_STATE)
         fprintf(f, "FSM states:");
      else if (kind == COV_ITEM_FUNCTIONAL)
         fprintf(f, "functional coverage:");
      fprintf(f, "</h2>\n");

      fprintf(f, "  <div style=\"padding:0px 10px;\">\n");
      cover_print_pairs(f, first_pair, pkind, n);
      fprintf(f, "  </div>\n");

      fprintf(f, "  </section>\n\n");
   }

   fprintf(f, "</div>\n");
}

static void cover_print_chns(FILE *f, cover_data_t *data, cover_chain_group_t *chns)
{
   fprintf(f, "<div class=\"tab\">"
              "   <button class=\"tablinks\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Branch')\">Branch</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Expression')\">Expression</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'FSM_state')\">FSM state</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Functional')\">Functional</button>\n"
              "</div>\n\n");

   cover_print_chain(f, data, &(chns->stmt), COV_ITEM_STMT);
   cover_print_chain(f, data, &(chns->branch), COV_ITEM_BRANCH);
   cover_print_chain(f, data, &(chns->toggle), COV_ITEM_TOGGLE);
   cover_print_chain(f, data, &(chns->expression), COV_ITEM_EXPRESSION);
   cover_print_chain(f, data, &(chns->state), COV_ITEM_STATE);
   cover_print_chain(f, data, &(chns->functional), COV_ITEM_FUNCTIONAL);
}

static void cover_print_jscript_funcs(FILE *f)
{
   fprintf(f, "<script>\n"
            "   function selectCoverage(evt, coverageType) {\n"
            "      var i, tabcontent, tablinks;\n"
            "      tabcontent = document.getElementsByClassName(\"tabcontent\");\n"
            "      for (i = 0; i < tabcontent.length; i++) {\n"
            "         tabcontent[i].style.display = \"none\";\n"
            "      }\n"
            "      tablinks = document.getElementsByClassName(\"tablinks\");\n"
            "      for (i = 0; i < tablinks.length; i++) {\n"
            "         tablinks[i].className = tablinks[i].className.replace(\" active\", \"\");\n"
            "      }\n"
            "      document.getElementById(coverageType).style.display = \"block\";\n"
            "      evt.currentTarget.className += \" active\";\n"
            "   }\n"
            "   function GetExclude(excludeCmd) {\n"
            "      navigator.clipboard.writeText(excludeCmd);\n"
            "   }\n"
            "   function getCellValue (tr, n) {\n"
            "      let v = tr.getElementsByTagName(\"TD\")[n];\n"
            "      v = parseInt(v.innerHTML.split(\"%%\")[0]);\n"
            "      if (isNaN(v)) {\n"
            "         v = -1;\n"
            "      }\n"
            "      return v;\n"
            "   }\n"
            "   function sortTable(n, tableId) {\n"
            "     const table = document.getElementById(tableId);\n"
            "     const rows = Array.from(table.querySelectorAll('tr:nth-child(n+2)'));\n"
            "     rows.sort((a, b) => {\n"
            "       const left = getCellValue(this.ascending ? a : b, n);\n"
            "       const right = getCellValue(this.ascending ? b : a, n);\n"
            "       if (!isNaN(left) && !isNaN(right)) {\n"
            "         return left - right;\n"
            "       } else {\n"
            "         return left.localeCompare(right);\n"
            "       }\n"
            "     });\n"
            "     rows.forEach(tr => table.appendChild(tr));\n"
            "     this.ascending = !this.ascending;\n"
            "   }\n"
            "   document.getElementById(\"defaultOpen\").click();\n"
            "</script>\n");
}

static bool cover_bin_unreachable(cover_data_t *data, cover_item_t *item)
{
   if ((data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Toggle items remember unreachability in run-time data. Must check item kind
   // not to get false unreachability on statement items. Excludes both bins
   // automatically!
   if (item->kind == COV_ITEM_TOGGLE && ((item->data & COV_FLAG_UNREACHABLE) != 0)) {
      item->flags |= COV_FLAG_UNREACHABLE;
      return true;
   }

   // Expression items remember unreachability as unreachable mask
   if (item->kind == COV_ITEM_EXPRESSION && ((item->flags & COV_FLAG_UNREACHABLE) != 0))
      return true;

   return false;
}

#define CHAIN_APPEND(chn, type, first_chn_item, curr_item, curr_line)            \
      do {                                                                       \
         if (chn->n_##type == chn->alloc_##type) {                               \
            chn->alloc_##type *= 2;                                              \
            chn->type = xrealloc_array(chn->type , chn->alloc_##type ,           \
                                       sizeof(cover_pair_t));                    \
         }                                                                       \
         chn->type[chn->n_##type].item = curr_item;                              \
         chn->type[chn->n_##type].line = curr_line;                              \
         chn->n_##type++;                                                        \
         /* Count consecutive items appended to the one single chain type */     \
         if (first_chn_item == NULL) {                                           \
            first_chn_item = curr_item;                                          \
            first_chn_item->consecutive = 1;                                     \
         }                                                                       \
         else                                                                    \
            first_chn_item->consecutive++;                                       \
      } while (0);


static int cover_append_item_to_chain(cover_data_t *data, cover_chain_group_t *chns,
                                      cover_stats_t *flat, cover_stats_t *nested,
                                      cover_item_t *first_item, cover_line_t *line,
                                      int *skipped)
{
   unsigned *flat_total   = NULL;
   unsigned *nested_total = NULL;
   unsigned *flat_hits    = NULL;
   unsigned *nested_hits  = NULL;
   cover_chain_t *chn;

   switch (first_item->kind) {
   case COV_ITEM_STMT:
      flat_total   = &(flat->total_stmts);
      flat_hits    = &(flat->hit_stmts);
      nested_total = &(nested->total_stmts);
      nested_hits  = &(nested->hit_stmts);
      chn = &(chns->stmt);
      break;
   case COV_ITEM_BRANCH:
      flat_total   = &(flat->total_branches);
      flat_hits    = &(flat->hit_branches);
      nested_total = &(nested->total_branches);
      nested_hits  = &(nested->hit_branches);
      chn = &(chns->branch);
      break;
   case COV_ITEM_TOGGLE:
      flat_total   = &(flat->total_toggles);
      flat_hits    = &(flat->hit_toggles);
      nested_total = &(nested->total_toggles);
      nested_hits  = &(nested->hit_toggles);
      chn = &(chns->toggle);
      break;
   case COV_ITEM_STATE:
      flat_total   = &(flat->total_states);
      flat_hits    = &(flat->hit_states);
      nested_total = &(nested->total_states);
      nested_hits  = &(nested->hit_states);
      chn = &(chns->state);
      break;
   case COV_ITEM_EXPRESSION:
      flat_total   = &(flat->total_expressions);
      flat_hits    = &(flat->hit_expressions);
      nested_total = &(nested->total_expressions);
      nested_hits  = &(nested->hit_expressions);
      chn = &(chns->expression);
      break;
   case COV_ITEM_FUNCTIONAL:
      flat_total = &(flat->total_functional);
      flat_hits = &(flat->hit_functional);
      nested_total = &(nested->total_functional);
      nested_hits = &(nested->hit_functional);
      chn = &(chns->functional);
      break;
   default:
      fatal("unsupported type of code coverage: %d at 'cover_append_item_to_chain'!",
             first_item->kind);
   }

   // Process all consecutive cover_items in belonging to the same RTL construct
   // "first_item->consecutive" gives the number of items to process since cover item emit.
   // Re-distribute so that this number is valid even if the items are sorted into
   // "hit", "miss" and "exclude" pair lists. The "consecutive" in the original cover_item
   // will get corrupted, but "consecutive" for each first item in each of the pair lists will
   // correspond to how many items within that given pair list correspond to the
   // same RTL construct. This re-sorting then simplifies coverage reporting.
   cover_item_t *first_hits_item = NULL;
   cover_item_t *first_miss_item = NULL;
   cover_item_t *first_excl_item = NULL;

   int n_steps = first_item->consecutive;
   cover_item_t *last_item = first_item + n_steps - 1;

   for (cover_item_t *curr_item = first_item; curr_item <= last_item; curr_item++) {
      (*flat_total)++;
      (*nested_total)++;

      if (curr_item->data >= curr_item->atleast && curr_item->atleast > 0) {
         (*flat_hits)++;
         (*nested_hits)++;

         if (chn->n_hits > data->report_item_limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, hits, first_hits_item, curr_item, line)
      }
      else if ((curr_item->flags & COV_FLAG_EXCLUDED) ||
                cover_bin_unreachable(data, curr_item)) {
         (*flat_hits)++;
         (*nested_hits)++;

         if (chn->n_excl > data->report_item_limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, excl, first_excl_item, curr_item, line)
      }
      else {
         if (chn->n_miss > data->report_item_limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, miss, first_miss_item, curr_item, line)
      }
   };

   return n_steps;
}


///////////////////////////////////////////////////////////////////////////////
// Per hierarchy reporting functions
///////////////////////////////////////////////////////////////////////////////

static void cover_report_hier_scope(cover_rpt_hier_ctx_t *ctx,
                                    cover_scope_t *s, const char *dir,
                                    FILE *summf, int *skipped)
{
   for (int i = 0; i < s->items.count;) {
      int step = 1;
      cover_item_t *item = &(s->items.items[i]);
      assert(item->loc.file_ref == s->loc.file_ref);

      cover_file_t *f_src = cover_file_for_scope(s);
      if (f_src == NULL) {
         i += 1;
         continue;
      }

      cover_line_t *line = &(f_src->lines[item->loc.first_line-1]);

      switch (item->kind) {
      case COV_ITEM_STMT:
      case COV_ITEM_BRANCH:
      case COV_ITEM_TOGGLE:
      case COV_ITEM_STATE:
      case COV_ITEM_EXPRESSION:
      case COV_ITEM_FUNCTIONAL:
         step = cover_append_item_to_chain(ctx->data, &(ctx->chns),
                                           &(ctx->flat_stats), &(ctx->nested_stats),
                                           item, line, skipped);
         break;

      default:
         fatal("unsupported type of code coverage: %d at 'cover_report_hier_scope'!",
               item->kind);
      }

      i += step;
   }

   cover_report_hier_children(ctx, s, dir, summf, skipped);
}

static void cover_print_summary_table_row(FILE *f, cover_data_t *data, cover_stats_t *stats,
                                          ident_t entry_name, ident_t entry_link, int lvl,
                                          bool top, bool print_out)
{
   fprintf(f, "  <tr>\n"
              "    <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(entry_link), istr(entry_name));

   cover_print_percents_cell(f, stats->hit_stmts, stats->total_stmts);
   cover_print_percents_cell(f, stats->hit_branches, stats->total_branches);
   cover_print_percents_cell(f, stats->hit_toggles, stats->total_toggles);
   cover_print_percents_cell(f, stats->hit_expressions, stats->total_expressions);
   cover_print_percents_cell(f, stats->hit_states, stats->total_states);
   cover_print_percents_cell(f, stats->hit_functional, stats->total_functional);

   int avg_total = stats->total_stmts + stats->total_branches +
                   stats->total_toggles + stats->total_expressions +
                   stats->total_states + stats->total_functional;
   int avg_hit = stats->hit_stmts + stats->hit_branches +
                 stats->hit_toggles + stats->hit_expressions +
                 stats->hit_states + stats->total_functional;

   cover_print_percents_cell(f, avg_hit, avg_total);

   fprintf(f, "  </tr>\n");

   float perc_stmt = 0.0f;
   float perc_branch = 0.0f;
   float perc_toggle = 0.0f;
   float perc_expr = 0.0f;
   float perc_state = 0.0f;
   float perc_functional = 0.0f;

   if (stats->total_stmts > 0)
      perc_stmt = 100.0 * ((float)stats->hit_stmts) / stats->total_stmts;
   if (stats->total_branches > 0)
      perc_branch = 100.0 * ((float)stats->hit_branches) / stats->total_branches;
   if (stats->total_toggles > 0)
      perc_toggle = 100.0 * ((float)stats->hit_toggles) / stats->total_toggles;
   if (stats->total_expressions > 0)
      perc_expr = 100.0 * ((float)stats->hit_expressions) / stats->total_expressions;
   if (stats->total_states > 0)
      perc_state = 100.0 * ((float)stats->hit_states) / stats->total_states;
   if (stats->total_functional > 0)
      perc_functional = 100.0 * ((float)stats->hit_functional) / stats->total_functional;

   if (top) {
      notef("code coverage results for: %s", istr(entry_name));

      if (stats->total_stmts > 0)
         notef("     statement:     %.1f %% (%d/%d)", perc_stmt,
               stats->hit_stmts, stats->total_stmts);
      else
         notef("     statement:     N.A.");

      if (stats->total_branches > 0)
         notef("     branch:        %.1f %% (%d/%d)", perc_branch,
               stats->hit_branches, stats->total_branches);
      else
         notef("     branch:        N.A.");

      if (stats->total_toggles > 0)
         notef("     toggle:        %.1f %% (%d/%d)", perc_toggle,
               stats->hit_toggles, stats->total_toggles);
      else
         notef("     toggle:        N.A.");

      if (stats->total_expressions > 0)
         notef("     expression:    %.1f %% (%d/%d)", perc_expr,
               stats->hit_expressions, stats->total_expressions);
      else
         notef("     expression:    N.A.");

      if (stats->total_states > 0)
         notef("     FSM state:     %.1f %% (%d/%d)", perc_state,
               stats->hit_states, stats->total_states);
      else
         notef("     FSM state:     N.A.");

      if (stats->total_functional > 0)
         notef("     functional:    %.1f %% (%d/%d)", perc_functional,
               stats->hit_functional, stats->total_functional);
      else
         notef("     functional:    N.A.");
   }
   else if (opt_get_int(OPT_VERBOSE) && print_out) {

      cover_rpt_buf_t *new = xcalloc(sizeof(cover_rpt_buf_t));
      new->tb = tb_new();
      new->prev = data->rpt_buf;
      data->rpt_buf = new;

      tb_printf(new->tb,
         "%*s %-*s %10.1f %% (%d/%d)  %10.1f %% (%d/%d) %10.1f %% (%d/%d) "
         "%10.1f %% (%d/%d) %10.1f %% (%d/%d)",
         lvl, "", 50-lvl, istr(ident_rfrom(entry_name, '.')),
         perc_stmt, stats->hit_stmts, stats->total_stmts,
         perc_branch, stats->hit_branches, stats->total_branches,
         perc_toggle, stats->hit_toggles, stats->total_toggles,
         perc_expr, stats->hit_expressions, stats->total_expressions,
         perc_state, stats->hit_states, stats->total_states);
   }
}

static char *cover_get_report_name(const char *in)
{
   SHA1_CTX ctx;
   unsigned char buf[SHA1_LEN];
   char *rv = xcalloc(2 * SHA1_LEN + 1);

   SHA1Init(&ctx);
   SHA1Update(&ctx, (const char unsigned*)in, strlen(in));
   SHA1Final(buf, &ctx);

   for (int i = 0; i < SHA1_LEN; i++)
      snprintf(rv + i * 2, 3, "%02x", buf[i]);

   return rv;
}

static void cover_print_hier_nav_tree(FILE *f, cover_scope_t *s)
{
   fprintf(f, "<nav>\n"
               "<b>Hierarchy:</b><br>\n");
   int offset = 0;

   ident_t full_hier = s->hier;
   ident_t curr_id;
   ident_t curr_hier = NULL;

   do {
      curr_id = ident_walk_selected(&full_hier);
      curr_hier = ident_prefix(curr_hier, curr_id, '.');
      const char *link LOCAL = (offset == 0) ? xstrdup("../index") :
                                               cover_get_report_name(istr(curr_hier));
      if (curr_id)
         fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                     offset * 10, link, istr(curr_id));
      offset++;
   } while (curr_id != NULL);

   fprintf(f, "</nav>\n\n");
}

static void cover_report_hier(cover_rpt_hier_ctx_t *ctx,
                              cover_scope_t *s, const char *dir)
{
   char *rpt_name LOCAL = cover_get_report_name(istr(s->hier));
   char *hier LOCAL = xasprintf("%s/%s.html", dir, rpt_name);

   FILE *f = fopen(hier, "w");
   if (f == NULL)
      fatal("failed to open report file: %s\n", hier);

   INIT_CHAIN(ctx, stmt);
   INIT_CHAIN(ctx, branch);
   INIT_CHAIN(ctx, toggle);
   INIT_CHAIN(ctx, expression);
   INIT_CHAIN(ctx, state);
   INIT_CHAIN(ctx, functional);

   cover_print_html_header(f);
   cover_print_hier_nav_tree(f, s);
   cover_print_inst_name(f, s);

   cover_file_t *src = cover_file_for_scope(s);
   const char *filename = (src) ? src->name : "";
   cover_print_file_name(f, filename);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Sub-instances:\n</h2>\n\n");
   cover_print_summary_table_header(f, "sub_inst_table", true);

   int skipped = 0;
   cover_report_hier_children(ctx, s, dir, f, &skipped);

   cover_print_table_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current Instance:\n</h2>\n\n");
   cover_print_summary_table_header(f, "cur_inst_table", true);
   ident_t rpt_name_id = ident_new(rpt_name);
   cover_print_summary_table_row(f, ctx->data, &(ctx->flat_stats), s->hier, rpt_name_id,
                                 ctx->lvl, false, false);
   cover_print_table_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h3>\n\n", ctx->data->report_item_limit, skipped);
   cover_print_chns(f, ctx->data, &(ctx->chns));
   cover_print_jscript_funcs(f);

   cover_print_timestamp(f);

   fclose(f);
}

static void cover_report_hier_children(cover_rpt_hier_ctx_t *ctx,
                                       cover_scope_t *s, const char *dir,
                                       FILE *summf, int *skipped)
{
   for (int i = 0; i < s->children.count; i++) {
      cover_scope_t *it = s->children.items[i];
      if (it->type == CSCOPE_INSTANCE) {
         // Collect coverage of sub-block
         cover_rpt_hier_ctx_t sub_ctx = {};
         sub_ctx.parent = ctx;
         sub_ctx.data = ctx->data;
         sub_ctx.lvl = ctx->lvl + 2;

         cover_report_hier(&sub_ctx, it, dir);

         const char *rpt_name LOCAL = cover_get_report_name(istr(it->hier));
         cover_print_summary_table_row(summf, ctx->data, &(sub_ctx.nested_stats),
                                       ident_rfrom(it->hier, '.'), ident_new(rpt_name),
                                       sub_ctx.lvl, false, true);

         // Add coverage from sub-hierarchies
         ctx->nested_stats.hit_stmts += sub_ctx.nested_stats.hit_stmts;
         ctx->nested_stats.total_stmts += sub_ctx.nested_stats.total_stmts;
         ctx->nested_stats.hit_branches += sub_ctx.nested_stats.hit_branches;
         ctx->nested_stats.total_branches += sub_ctx.nested_stats.total_branches;
         ctx->nested_stats.hit_toggles += sub_ctx.nested_stats.hit_toggles;
         ctx->nested_stats.total_toggles += sub_ctx.nested_stats.total_toggles;
         ctx->nested_stats.hit_expressions += sub_ctx.nested_stats.hit_expressions;
         ctx->nested_stats.total_expressions += sub_ctx.nested_stats.total_expressions;
         ctx->nested_stats.hit_states += sub_ctx.nested_stats.hit_states;
         ctx->nested_stats.total_states += sub_ctx.nested_stats.total_states;
         ctx->nested_stats.hit_functional += sub_ctx.nested_stats.hit_functional;
         ctx->nested_stats.total_functional += sub_ctx.nested_stats.total_functional;
      }
      else
         cover_report_hier_scope(ctx, it, dir, summf, skipped);
   }
}

static void cover_report_per_hier(FILE *f, cover_data_t *data, char *subdir)
{
   for (int i = 0; i < data->root_scope->children.count; i++) {
      cover_scope_t *child = AGET(data->root_scope->children, i);
      cover_rpt_hier_ctx_t top_ctx = {};

      top_ctx.data = data;

      cover_report_hier(&top_ctx, child, subdir);
      char *rpt_link LOCAL = cover_get_report_name(istr(child->hier));
      cover_print_summary_table_row(f, data, &(top_ctx.nested_stats), child->hier,
                                    ident_new(rpt_link), top_ctx.lvl, true, true);
   }

   if (opt_get_int(OPT_VERBOSE)) {
      notef("Coverage for sub-hierarchies:");
      printf("%-55s %-20s %-20s %-20s %-20s %-20s\n",
             "Hierarchy", "Statement", "Branch", "Toggle", "Expression", "FSM state");
      cover_rpt_buf_t *buf = data->rpt_buf;
      while (buf) {
         printf("%s\n", tb_get(buf->tb));
         tb_free(buf->tb);
         data->rpt_buf = buf->prev;
         free(buf);
         buf = data->rpt_buf;
      };
   }

   cover_print_table_footer(f);
}

///////////////////////////////////////////////////////////////////////////////
// Per source file reporting functions
///////////////////////////////////////////////////////////////////////////////

static void cover_print_file_nav_tree(FILE *f, cover_rpt_file_ctx_t *ctx_list,
                                      int n_ctxs)
{
   fprintf(f, "<nav>\n");
   fprintf(f, "<b><a href=../index.html>Back to summary</a></p></b>\n");
   fprintf(f, "<b>Files:</b><br>\n");

   for (int i = 0; i < n_ctxs; i++) {
      cover_rpt_file_ctx_t *ctx = ctx_list + i;

      char *tmp LOCAL = xstrdup((char *)ctx->file->name);
      const char *file_name = basename(tmp);
      fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                  10, file_name, file_name);
   }

   fprintf(f, "</nav>\n\n");
}

static cover_rpt_file_ctx_t *cover_rpt_file_collect_scope(
                                 cover_data_t *data,
                                 cover_rpt_file_ctx_t *ctx_list,
                                 cover_scope_t *s,
                                 int *n_ctxs,
                                 int *alloc_ctxs)
{
   cover_file_t *scope_f = cover_file_for_scope(s);

   if (scope_f) {
      // Look if data exist for this source file
      cover_rpt_file_ctx_t *curr_ctx = NULL;
      for (cover_rpt_file_ctx_t *it = ctx_list; it < ctx_list + (*n_ctxs); it++)
         if (it->file == scope_f) {
            curr_ctx = it;
            break;
         }

      // Create new per-file context if not
      if (curr_ctx == NULL) {
         if (*n_ctxs == *alloc_ctxs) {
            *alloc_ctxs *= 2;
            ctx_list = xrealloc_array(ctx_list, *alloc_ctxs,
                                    sizeof(cover_rpt_file_ctx_t));
         }

         curr_ctx = ctx_list + *n_ctxs;
         curr_ctx->file = scope_f;
         (*n_ctxs)++;
      }

      int l_size = curr_ctx->items.count;

      // Upon first scope being placed into the file, copy directly.
      // This optimizes for use-case where there is single scope with many
      // toggle coverage items (e.g. include-mems), no need to iterate!
      // If multiple instantiations of instance with such toggle items is done,
      // then this will not help.
      if (l_size == 0) {
         for (int i = 0; i < s->items.count; i++)
            APUSH(curr_ctx->items, AREF(s->items, i));
      }
      else {
         // Walk scope items and check if they exist in file items.
         // Merge if yes, append if no.
         // TODO: This has O(n^2). May be issue for large designs ?
         for (int i = 0; i < s->items.count; i++) {
            cover_item_t *scope_item = AREF(s->items, i);

            bool found = false;

            for (int i = 0; i < curr_ctx->items.count; i++) {
               cover_item_t *file_item = curr_ctx->items.items[i];

               // We must take into account:
               //    - kind   - different kind items can be at the same loc
               //    - loc    - to get aggregated per-file data
               //    - flags  - to not merge different bins
               if ((file_item->kind == scope_item->kind) &&
                  loc_eq(&(file_item->loc), &(scope_item->loc)) &&
                  (file_item->flags == scope_item->flags)) {
                  cover_merge_one_item(file_item, scope_item->data);
                  found = true;
                  break;
               }
            }

            if (!found)
               APUSH(curr_ctx->items, scope_item);
         }
      }
   }

   // Process nested scopes
   for (int i = 0; i < s->children.count; i++) {
      cover_scope_t *child = AGET(s->children, i);
      ctx_list = cover_rpt_file_collect_scope(data, ctx_list, child, n_ctxs,
                                              alloc_ctxs);
   }

   return ctx_list;
}

static void cover_report_per_file(FILE *top_f, cover_data_t *data, char *subdir)
{
   int alloc_ctxs = 32;
   int n_ctxs = 0;
   cover_rpt_file_ctx_t *ctx_list = xcalloc_array(alloc_ctxs,
                                                  sizeof(cover_rpt_file_ctx_t));

   // Traverse hierarchy and merge all items into "per-file" lists
   for (int i = 0; i < data->root_scope->children.count; i++) {
      cover_scope_t *child = AGET(data->root_scope->children, i);
      ctx_list = cover_rpt_file_collect_scope(data, ctx_list, child,
                                              &n_ctxs, &alloc_ctxs);
   }

   // Convert to chains and print
   for (int i = 0; i < n_ctxs; i++) {
      cover_rpt_file_ctx_t *ctx = ctx_list + i;

      if (ctx->file == NULL) {
         i += 1;
         continue;
      }

      INIT_CHAIN(ctx, stmt);
      INIT_CHAIN(ctx, branch);
      INIT_CHAIN(ctx, toggle);
      INIT_CHAIN(ctx, expression);
      INIT_CHAIN(ctx, state);
      INIT_CHAIN(ctx, functional);

      int skipped = 0;

      for (int j = 0; j < ctx->items.count;) {
         int step = 1;
         cover_item_t *item = ctx->items.items[j];
         cover_line_t *line = &(ctx->file->lines[item->loc.first_line-1]);
         cover_stats_t nested;

         switch (item->kind) {
         case COV_ITEM_STMT:
         case COV_ITEM_BRANCH:
         case COV_ITEM_TOGGLE:
         case COV_ITEM_STATE:
         case COV_ITEM_EXPRESSION:
         case COV_ITEM_FUNCTIONAL:
            step = cover_append_item_to_chain(data, &(ctx->chns), &(ctx->stats), &nested,
                                              item, line, &skipped);
            break;

         default:
            fatal("unsupported type of code coverage: %d at 'cover_report_per_file'!",
                   item->kind);
         }

         j += step;
      }

      // Print per-file report
      char *file_name LOCAL = xstrdup((char*)ctx->file->name);
      ident_t base_name_id = ident_new(basename(file_name));
      char *file_path LOCAL = xasprintf("%s/%s.html", subdir, istr(base_name_id));

      FILE *f = fopen(file_path, "w");
      if (f == NULL)
         fatal_errno("failed to open report file: %s\n", file_path);


      cover_print_html_header(f);
      cover_print_file_nav_tree(f, ctx_list, n_ctxs);
      cover_print_file_name(f, ctx->file->name);

      fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current File:\n</h2>\n\n");
      cover_print_summary_table_header(f, "cur_file_table", true);
      cover_print_summary_table_row(f, data, &(ctx->stats), base_name_id, base_name_id,
                                    0, false, false);
      cover_print_table_footer(f);

      fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");
      if (skipped)
         fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                    "printed items was reached (%d). Total %d items are not "
                    "displayed.</h3>\n\n", data->report_item_limit, skipped);
      cover_print_chns(f, data, &(ctx->chns));
      cover_print_jscript_funcs(f);

      cover_print_timestamp(f);

      // Print top table summary
      cover_print_summary_table_row(top_f, data, &(ctx->stats), base_name_id, base_name_id,
                                    0, true, false);
   }

   cover_print_table_footer(top_f);
   cover_print_jscript_funcs(top_f);
}

///////////////////////////////////////////////////////////////////////////////
// Global API
///////////////////////////////////////////////////////////////////////////////

void cover_report(const char *path, cover_data_t *data, int item_limit)
{
   char *subdir LOCAL = xasprintf("%s/hier", path);
   make_dir(path);
   make_dir(subdir);

   static const struct {
      const char *name;
      cover_mask_t mask;
   } lst[] = {
      { "covered",      COVER_MASK_DONT_PRINT_COVERED       },
      { "uncovered",    COVER_MASK_DONT_PRINT_UNCOVERED     },
      { "excluded",     COVER_MASK_DONT_PRINT_EXCLUDED      },
   };

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "Code coverage report contains: ");

   bool first = true;
   for (int i = 0; i < ARRAY_LEN(lst); i++)
      if (!cover_enabled(data, lst[i].mask)) {
         if (first)
            first = false;
         else
            tb_printf(tb, ", ");
         tb_printf(tb, "%s", lst[i].name);
      }
   tb_printf(tb, " coverage details.");

   notef("Code coverage report folder: %s.", path);
   notef("%s", tb_get(tb));

   char *top LOCAL = xasprintf("%s/index.html", path);
   FILE *f = fopen(top, "w");

   cover_print_html_header(f);

   data->report_item_limit = item_limit;

   if (data->mask & COVER_MASK_PER_FILE_REPORT) {
      cover_print_summary_table_header(f, "file_table", false);
      cover_report_per_file(f, data, subdir);
   }
   else {
      cover_print_summary_table_header(f, "inst_table", true);
      cover_report_per_hier(f, data, subdir);
   }

   cover_print_timestamp(f);

   fclose(f);
}
