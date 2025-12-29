//
//  Copyright (C) 2013-2025  Nick Gasson
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
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "ident.h"
#include "option.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <libgen.h>
#include <inttypes.h>
#include <math.h>

#define MARGIN_LEFT "20%%"
#define SIDEBAR_WIDTH "15%%"

struct _cover_rpt_buf {
   text_buf_t      *tb;
   cover_rpt_buf_t *prev;
};

typedef enum {
   PAIR_UNCOVERED    = 0,
   PAIR_EXCLUDED     = 1,
   PAIR_COVERED      = 2,
   PAIR_LAST         = 3
} cov_pair_kind_t;

typedef struct {
   cover_rpt_t  *rpt;
   cover_data_t *data;
   const char   *outdir;
   unsigned      item_limit;
} html_gen_t;

#define COV_RPT_TITLE "NVC code coverage report"

static void cover_report_hier_children(html_gen_t *g, int lvl,
                                       cover_scope_t *s, FILE *summf);
static void cover_print_html_header(FILE *f);
static inline void cover_print_char(FILE *f, char c);

///////////////////////////////////////////////////////////////////////////////
// Common reporting functions
///////////////////////////////////////////////////////////////////////////////

static void cover_print_html_header(FILE *f)
{
   fprintf(f, "<!DOCTYPE html>\n"
           "<html lang=\"en\">\n"
           "<head>\n"
           "  <meta charset=\"utf-8\">\n"
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
              "   }\n"
              "   table {\n"
              "     table-layout: fixed;"
              "   }\n"
              "   table, th, td {\n"
              "     border: 2px solid black;\n"
              "     border-collapse: collapse;\n"
              "     word-wrap: break-word;\n"
              "   }\n"
              "   nav details details {\n"
              "     margin-left: 1rem;\n"
              "   }\n"
              "   nav details > a {\n"
              "     padding-left: 1.9rem;\n"
              "     display: block;\n"
              "   }\n"
              "   nav summary {\n"
              "     display: inline-flex;\n"
              "     cursor: pointer;\n"
              "     align-items: center;\n"
              "   }\n"
              "   nav summary::before {\n"
              "     content: '▶';\n"
              "     font-size: 0.7em;\n"
              "     margin-right: 0.4em;\n"
              "   }\n"
              "   nav details[open] > summary::before {\n"
              "     transform: rotate(90deg);\n"
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
              "   .nav-sel { font-weight: bold; }\n"
              "  </style>\n"
              "</head>\n"
              "<body>\n\n");

   fprintf(f, "<header>");
   fprintf(f, COV_RPT_TITLE "\n");
   fprintf(f, "</header>\n\n");
}

static void cover_print_file_name(FILE *f, const rpt_file_t *src)
{
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   fprintf(f, "   File:&nbsp; <a href=\"../source/%s.html\">%s</a>\n",
           src ? src->path_hash : "", src ? src->path : "");
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
      float perc = (floor(((float) hit / (float) total) * 1000)) / 10;
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
   time_t timestamp;
   const long override_time = opt_get_int(OPT_COVER_TIMESTAMP);
   if (override_time >= 0)
      timestamp = override_time;
   else
      timestamp = time(NULL);

   fprintf(f, "<footer>");
   fprintf(f, "   <p> NVC version: %s </p>\n", PACKAGE_VERSION);
   fprintf(f, "   <p> Generated on: %s </p>\n", ctime(&timestamp));
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

static void cover_print_string(FILE *f, const char *s)
{
   while (*s != '\0')
      cover_print_char(f, *s++);
}

static void cover_print_single_code_line(FILE *f, loc_t loc,
                                         const rpt_line_t *line)
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

static void cover_print_lhs_rhs_arrows(FILE *f, rpt_pair_t *pair)
{
   int last = strlen(pair->line->text);
   int curr = 0;

   // Offset by line number width
   int digits = pair->item->loc.first_line;
   do {
      digits /= 10;
      fprintf(f, "&nbsp;");
   } while (digits > 0);
   fprintf(f, "&nbsp;");

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
         fprintf(f, "&lt;");
      else if (curr > lhs_beg && curr < lhs_end)
         fprintf(f, "-");
      else if (curr == (lhs_beg + loc_lhs->column_delta))
         fprintf(f, "&gt;");

      else if (curr == rhs_mid - 1)
         fprintf(f, "R");
      else if (curr == rhs_mid)
         fprintf(f, "H");
      else if (curr == rhs_mid + 1)
         fprintf(f, "S");

      else if (curr == rhs_beg)
         fprintf(f, "&lt;");
      else if (curr > rhs_beg && curr < rhs_end)
         fprintf(f, "-");
      else if (curr == (rhs_beg + loc_rhs->column_delta))
         fprintf(f, "&gt;");

      else
         fprintf(f, "&nbsp;");

      curr++;
   }
}

static void cover_print_item_title(FILE *f, rpt_pair_t *pair)
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
      fprintf(f, "%s:", text[pair->item->source]);
      break;
   case COV_ITEM_EXPRESSION:
      cover_print_string(f, istr(pair->item->func_name));
      fprintf(f, " expression");
      break;
   case COV_ITEM_STATE:
      fprintf(f, "\"%s\" FSM", istr(pair->item->func_name));
      break;
   default:
      break;
   }
   fprintf(f, "</h3>");
}

static void cover_print_code_loc(FILE *f, rpt_pair_t *pair)
{
   loc_t loc = pair->item->loc;
   const rpt_line_t *curr_line = pair->line;
   const rpt_line_t *last_line = pair->line + loc.line_delta;

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

static void cover_print_bin(FILE *f, rpt_pair_t *pair, uint32_t flag,
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

static void cover_print_bins(FILE *f, rpt_pair_t *first_pair,
                             cov_pair_kind_t pkind, int consecutive)
{
   rpt_pair_t *last_pair = first_pair + consecutive - 1;
   rpt_pair_t *pair = first_pair;
   for (; pair <= last_pair; pair++)
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
            int last = (loc.line_delta) ? first_pair->line->len :
                                          loc.column_delta + curr;

            LOCAL_TEXT_BUF tb = tb_new();
            tb_printf(tb, "<code>");
            while (curr <= last) {
               tb_printf(tb, "%c", first_pair->line->text[curr]);
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

static void cover_print_pairs(FILE *f, rpt_pair_t *first, cov_pair_kind_t pkind,
                              int pair_cnt)
{
   if (pair_cnt == 0)
      return;

   rpt_pair_t *last = first + pair_cnt - 1;
   rpt_pair_t *curr = first;
   do {
      if (curr > first) fprintf(f, "<hr>");

      assert(curr->line != NULL);

      int consecutive = 1;
      while (curr + consecutive <= last && curr[consecutive].line == NULL)
         consecutive++;

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

         cover_print_bins(f, curr, pkind, consecutive);
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

         cover_print_bins(f, curr, pkind, consecutive);
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

         cover_print_bins(f, curr, pkind, consecutive);
         break;
      }

      case COV_ITEM_STATE:
         cover_print_item_title(f, curr);
         cover_print_code_loc(f, curr);

         const char *title = "State";
         cover_print_bin_header(f, pkind, 1, &title);

         cover_print_bins(f, curr, pkind, consecutive);
         break;

      case COV_ITEM_FUNCTIONAL:
         if (curr->item->source == COV_SRC_USER_COVER) {
            cover_print_item_title(f, curr);
            fprintf(f, "<br>%s", istr(curr->item->func_name));

            const char *title[curr->item->n_ranges] LOCAL;

            for (int i = 0; i < curr->item->n_ranges; i++)
               title[i] = xasprintf("Variable %d", i);

            cover_print_bin_header(f, pkind, curr->item->n_ranges, title);
            cover_print_bins(f, curr, pkind, consecutive);
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

      curr += consecutive;
   } while (curr <= last);

   fprintf(f, "<div style=\"height:10px\"></div>\n");
}

static void cover_print_chain(FILE *f, cover_data_t *data, const rpt_chain_t *chn,
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
      rpt_pair_t *first_pair;

      if (pkind == PAIR_UNCOVERED) {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_UNCOVERED))
            continue;
         first_pair = chn->miss.items;
         n = chn->miss.count;
      }
      else if (pkind == PAIR_EXCLUDED) {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_EXCLUDED))
            continue;
         first_pair = chn->excl.items;
         n = chn->excl.count;
      }
      else {
         if (cover_enabled(data, COVER_MASK_DONT_PRINT_COVERED))
            continue;
         first_pair = chn->hits.items;
         n = chn->hits.count;
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

static void cover_print_chns(FILE *f, cover_data_t *data, const rpt_chain_group_t *chns)
{
   fprintf(f, "<div class=\"tab\">"
              "   <button class=\"tablinks\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Branch')\">Branch</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Expression')\">Expression</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'FSM_state')\">FSM state</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Functional')\">Functional</button>\n"
              "</div>\n\n");

   for (int i = 0; i < ARRAY_LEN(chns->chain); i++)
      cover_print_chain(f, data, &(chns->chain[i]), i);
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

///////////////////////////////////////////////////////////////////////////////
// Per hierarchy reporting functions
///////////////////////////////////////////////////////////////////////////////

static void cover_print_summary_table_row(FILE *f, cover_data_t *data, const rpt_stats_t *stats,
                                          ident_t entry_name, ident_t entry_link, int lvl,
                                          bool top, bool print_out)
{
   fprintf(f, "  <tr>\n"
              "    <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(entry_link), istr(entry_name));

   for (int i = 0; i <= COV_ITEM_FUNCTIONAL; i++)
      cover_print_percents_cell(f, stats->hit[i], stats->total[i]);

   int avg_total = 0, avg_hit = 0;
   for (int i = 0; i <= COV_ITEM_FUNCTIONAL; i++) {
      avg_total += stats->total[i];
      avg_hit += stats->hit[i];
   }

   cover_print_percents_cell(f, avg_hit, avg_total);

   fprintf(f, "  </tr>\n");

   float perc_stmt = 0.0f;
   float perc_branch = 0.0f;
   float perc_toggle = 0.0f;
   float perc_expr = 0.0f;
   float perc_state = 0.0f;
   float perc_functional = 0.0f;
   float perc_average = 0.0f;

   if (stats->total[COV_ITEM_STMT] > 0)
      perc_stmt = 100.0 * ((float)stats->hit[COV_ITEM_STMT]) / stats->total[COV_ITEM_STMT];
   if (stats->total[COV_ITEM_BRANCH] > 0)
      perc_branch = 100.0 * ((float)stats->hit[COV_ITEM_BRANCH]) / stats->total[COV_ITEM_BRANCH];
   if (stats->total[COV_ITEM_TOGGLE] > 0)
      perc_toggle = 100.0 * ((float)stats->hit[COV_ITEM_TOGGLE]) / stats->total[COV_ITEM_TOGGLE];
   if (stats->total[COV_ITEM_EXPRESSION] > 0)
      perc_expr = 100.0 * ((float)stats->hit[COV_ITEM_EXPRESSION]) / stats->total[COV_ITEM_EXPRESSION];
   if (stats->total[COV_ITEM_STATE] > 0)
      perc_state = 100.0 * ((float)stats->hit[COV_ITEM_STATE]) / stats->total[COV_ITEM_STATE];
   if (stats->total[COV_ITEM_FUNCTIONAL] > 0)
      perc_functional = 100.0 * ((float)stats->hit[COV_ITEM_FUNCTIONAL]) / stats->total[COV_ITEM_FUNCTIONAL];

   if (avg_total > 0)
      perc_average = 100.0 * ((float)avg_hit) / avg_total;

   if (top) {
      notef("code coverage results for: %s", istr(entry_name));

      if (stats->total[COV_ITEM_STMT] > 0)
         notef("     statement:     %.1f %% (%d/%d)", perc_stmt,
               stats->hit[COV_ITEM_STMT], stats->total[COV_ITEM_STMT]);
      else
         notef("     statement:     N.A.");

      if (stats->total[COV_ITEM_BRANCH] > 0)
         notef("     branch:        %.1f %% (%d/%d)", perc_branch,
               stats->hit[COV_ITEM_BRANCH], stats->total[COV_ITEM_BRANCH]);
      else
         notef("     branch:        N.A.");

      if (stats->total[COV_ITEM_TOGGLE] > 0)
         notef("     toggle:        %.1f %% (%d/%d)", perc_toggle,
               stats->hit[COV_ITEM_TOGGLE], stats->total[COV_ITEM_TOGGLE]);
      else
         notef("     toggle:        N.A.");

      if (stats->total[COV_ITEM_EXPRESSION] > 0)
         notef("     expression:    %.1f %% (%d/%d)", perc_expr,
               stats->hit[COV_ITEM_EXPRESSION], stats->total[COV_ITEM_EXPRESSION]);
      else
         notef("     expression:    N.A.");

      if (stats->total[COV_ITEM_STATE] > 0)
         notef("     FSM state:     %.1f %% (%d/%d)", perc_state,
               stats->hit[COV_ITEM_STATE], stats->total[COV_ITEM_STATE]);
      else
         notef("     FSM state:     N.A.");

      if (stats->total[COV_ITEM_FUNCTIONAL] > 0)
         notef("     functional:    %.1f %% (%d/%d)", perc_functional,
               stats->hit[COV_ITEM_FUNCTIONAL], stats->total[COV_ITEM_FUNCTIONAL]);
      else
         notef("     functional:    N.A.");

      notef("     average:       %.1f %% (%d/%d)", perc_average, avg_hit, avg_total);
   }
   else if (opt_get_int(OPT_VERBOSE) && print_out) {

      cover_rpt_buf_t *new = xcalloc(sizeof(cover_rpt_buf_t));
      new->tb = tb_new();
      new->prev = data->rpt_buf;
      data->rpt_buf = new;

      tb_printf(new->tb,
         "%*s %-*s %10.1f %% (%6d / %6d)  %10.1f %% (%6d / %6d) %10.1f %% (%6d / %6d) "
         "%10.1f %% (%6d / %6d) %10.1f %% (%6d / %6d) %10.1f %% (%6d / %6d) %10.1f %% (%6d / %6d)",
         lvl, "", 50-lvl, istr(ident_rfrom(entry_name, '.')),
         perc_stmt, stats->hit[COV_ITEM_STMT], stats->total[COV_ITEM_STMT],
         perc_branch, stats->hit[COV_ITEM_BRANCH], stats->total[COV_ITEM_BRANCH],
         perc_toggle, stats->hit[COV_ITEM_TOGGLE], stats->total[COV_ITEM_TOGGLE],
         perc_expr, stats->hit[COV_ITEM_EXPRESSION], stats->total[COV_ITEM_EXPRESSION],
         perc_state, stats->hit[COV_ITEM_STATE], stats->total[COV_ITEM_STATE],
         perc_functional, stats->hit[COV_ITEM_FUNCTIONAL], stats->total[COV_ITEM_FUNCTIONAL],
         perc_average, avg_hit, avg_total);
   }
}

static void cover_print_nav_hier_node(html_gen_t *g, FILE *f, cover_scope_t *s,
                                      cover_scope_t *sel)
{
   const char *link = rpt_get_hier(g->rpt, s)->name_hash;

   bool open = false;
   for (cover_scope_t *it = sel; !open && it != NULL; it = it->parent)
      open |= (it == s);

   const bool leaf = cover_is_leaf(s);
   if (!leaf) {
      fprintf(f, "<details%s>\n", open ? " open" : "");
      fprintf(f, "<summary>");
   }

   fprintf(f, "<a href=\"%s.html\"", link);
   if (s == sel)
      fprintf(f, " class=\"nav-sel\"");
   fprintf(f, ">%s</a>\n", istr(s->name));

   if (!leaf) {
      fprintf(f, "</summary>\n");

      for (int i = 0; i < s->children.count; i++) {
         cover_scope_t *c = s->children.items[i];
         if (cover_is_hier(c))
            cover_print_nav_hier_node(g, f, c, sel);
      }

      fprintf(f, "</details>\n");
   }
}

static void cover_print_hier_nav_tree(html_gen_t *g, FILE *f, cover_scope_t *s)
{
   fprintf(f, "<nav>\n<b>Hierarchy:</b><br>\n");
   fprintf(f, "<details open>\n");
   fprintf(f, "<summary><a href=\"../index.html\">%s</a></summary>\n",
           istr(g->data->root_scope->name));

   for (int i = 0; i < g->data->root_scope->children.count; i++) {
      cover_scope_t *c = g->data->root_scope->children.items[i];
      cover_print_nav_hier_node(g, f, c, s);
   }

   fprintf(f, "</details>\n");
   fprintf(f, "</nav>\n\n");
}

static void cover_report_hier(html_gen_t *g, int lvl, cover_scope_t *s)
{
   const rpt_hier_t *h = rpt_get_hier(g->rpt, s);

   FILE *f = create_file("%s/hier/%s.html", g->outdir, h->name_hash);

   cover_print_html_header(f);
   cover_print_hier_nav_tree(g, f, s);
   cover_print_inst_name(f, s);

   const rpt_file_t *src = rpt_get_file(g->rpt, s);
   cover_print_file_name(f, src);

   if (!cover_is_leaf(s)) {
      fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Sub-instances:\n</h2>\n\n");
      cover_print_summary_table_header(f, "sub_inst_table", true);

      cover_report_hier_children(g, lvl, s, f);

      cover_print_table_footer(f);
   }

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current Instance:\n</h2>\n\n");
   cover_print_summary_table_header(f, "cur_inst_table", true);

   ident_t rpt_name_id = ident_new(h->name_hash);
   cover_print_summary_table_row(f, g->data, &(h->flat_stats), s->hier,
                                 rpt_name_id, lvl, false, false);
   cover_print_table_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");

   const int skipped = rpt_get_skipped(g->rpt);
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h3>\n\n", g->item_limit, skipped);
   cover_print_chns(f, g->data, &(h->chns));
   cover_print_jscript_funcs(f);

   cover_print_timestamp(f);

   fclose(f);
}

static void cover_report_hier_children(html_gen_t *g, int lvl,
                                       cover_scope_t *s, FILE *summf)
{
   for (int i = 0; i < s->children.count; i++) {
      cover_scope_t *it = s->children.items[i];
      if (cover_is_hier(it)) {
         cover_report_hier(g, lvl + 2, it);

         const rpt_hier_t *h = rpt_get_hier(g->rpt, it);

         cover_print_summary_table_row(summf, g->data, &(h->nested_stats),
                                       ident_rfrom(it->hier, '.'),
                                       ident_new(h->name_hash),
                                       lvl + 2, false, true);
      }
      else
         cover_report_hier_children(g, lvl, it, summf);
   }
}

static void cover_report_per_hier(html_gen_t *g, FILE *f, cover_data_t *data,
                                  cover_rpt_t *rpt)
{
   for (int i = 0; i < data->root_scope->children.count; i++) {
      cover_scope_t *child = AGET(data->root_scope->children, i);

      cover_report_hier(g, 0, child);

      const rpt_hier_t *h = rpt_get_hier(rpt, child);
      cover_print_summary_table_row(f, data, &(h->nested_stats), child->hier,
                                    ident_new(h->name_hash), 0, true, true);
   }

   if (opt_get_int(OPT_VERBOSE)) {
      notef("Coverage for sub-hierarchies:");
      printf("%-65s %-30s %-30s %-30s %-30s %-30s %-30s %-30s\n",
             "Hierarchy", "Statement", "Branch", "Toggle", "Expression", "FSM state", "Functional", "Average");
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

static void cover_print_file_nav_tree(FILE *f, int n_files,
                                      const rpt_file_t *files[n_files])
{
   fprintf(f, "<nav>\n");
   fprintf(f, "<b><a href=../index.html>Back to summary</a></p></b>\n");
   fprintf(f, "<b>Coverage report for file:</b><br>\n");

   for (int i = 0; i < n_files; i++) {
      char *tmp LOCAL = xstrdup((char *)files[i]->path);
      const char *file_name = basename(tmp);
      fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                  10, file_name, file_name);
   }

   fprintf(f, "</nav>\n\n");
}

static void cover_store_file_cb(const rpt_file_t *f, void *ctx)
{
   const rpt_file_t ***p = ctx;
   *(*p)++ = f;
}

static int cover_sort_files_cb(const void *a, const void *b)
{
   const rpt_file_t *fa = a, *fb = b;
   return strcmp(fa->path, fb->path);
}

static void cover_report_per_file(html_gen_t *g, FILE *top_f,
                                  cover_data_t *data, cover_rpt_t *rpt)
{
   const int n_files = rpt_iter_files(rpt, NULL, NULL);
   const rpt_file_t **files LOCAL =
      xmalloc_array(n_files, sizeof(rpt_file_t *)), **p = files;

   rpt_iter_files(rpt, cover_store_file_cb, &p);
   assert(p == files + n_files);

   qsort(files, n_files, sizeof(rpt_file_t *), cover_sort_files_cb);

   // Convert to chains and print
   for (int i = 0; i < n_files; i++) {
      // Print per-file report
      char *file_name LOCAL = xstrdup(files[i]->path);
      ident_t base_name_id = ident_new(basename(file_name));

      FILE *f = create_file("%s/hier/%s.html", g->outdir, istr(base_name_id));

      cover_print_html_header(f);
      cover_print_file_nav_tree(f, n_files, files);
      cover_print_file_name(f, files[i]);

      fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current File:\n</h2>\n\n");
      cover_print_summary_table_header(f, "cur_file_table", true);
      cover_print_summary_table_row(f, data, &(files[i]->stats), base_name_id,
                                    base_name_id, 0, false, false);
      cover_print_table_footer(f);

      fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");

      const int skipped = rpt_get_skipped(rpt);
      if (skipped)
         fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                    "printed items was reached (%d). Total %d items are not "
                    "displayed.</h3>\n\n", g->item_limit, skipped);
      cover_print_chns(f, data, &(files[i]->chns));
      cover_print_jscript_funcs(f);

      cover_print_timestamp(f);

      // Print top table summary
      cover_print_summary_table_row(top_f, data, &(files[i]->stats),
                                    base_name_id, base_name_id,
                                    0, true, false);
   }

   cover_print_table_footer(top_f);
   cover_print_jscript_funcs(top_f);
}

static void cover_file_page_cb(const rpt_file_t *f, void *ctx)
{
   html_gen_t *g = ctx;

   FILE *fp = create_file("%s/source/%s.html", g->outdir, f->path_hash);

   cover_print_html_header(fp);

   fprintf(fp, "<h2 style=\"text-align: left;\">\n");
   fprintf(fp, "   File:&nbsp; %s\n", f->path);
   fprintf(fp, "</h2>");

   fprintf(fp, "<pre><code>");
   for (int i = 0; i < f->n_lines; i++) {
      fprintf(fp, "%6d: &nbsp; ", i);
      for (const char *p = f->lines[i].text; *p; p++)
         cover_print_char(fp, *p);
      fprintf(fp, "\n");
   }
   fprintf(fp, "</code></pre>");

   fclose(fp);
}

///////////////////////////////////////////////////////////////////////////////
// Global API
///////////////////////////////////////////////////////////////////////////////

void cover_report(const char *path, cover_data_t *data, int item_limit)
{
   char *subdir LOCAL = xasprintf("%s/hier", path);
   make_dir("%s", path);
   make_dir("%s/hier", path);
   make_dir("%s/source", path);

   cover_rpt_t *rpt = cover_report_new(data, item_limit);

   html_gen_t g = {
      .data       = data,
      .rpt        = rpt,
      .outdir     = path,
      .item_limit = item_limit,
   };

   rpt_iter_files(rpt, cover_file_page_cb, &g);

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
   for (int i = 0; i < ARRAY_LEN(lst); i++) {
      if (!cover_enabled(data, lst[i].mask)) {
         if (first)
            first = false;
         else
            tb_printf(tb, ", ");
         tb_printf(tb, "%s", lst[i].name);
      }
   }
   tb_printf(tb, " coverage details.");

   notef("Code coverage report folder: %s.", path);
   notef("%s", tb_get(tb));

   FILE *f = create_file("%s/index.html", path);

   cover_print_html_header(f);

   if (data->mask & COVER_MASK_PER_FILE_REPORT) {
      cover_print_summary_table_header(f, "file_table", false);
      cover_report_per_file(&g, f, data, rpt);
   }
   else {
      cover_print_summary_table_header(f, "inst_table", true);
      cover_report_per_hier(&g, f, data, rpt);
   }

   cover_print_timestamp(f);

   cover_report_free(rpt);
   fclose(f);
}
