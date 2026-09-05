//
//  Copyright (C) 2013-2026  Nick Gasson
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
#include "cov/cov-priv.h"
#include "cov/cov-structs.h"
#include "cov/cov-style.h"
#include "ident.h"
#include "option.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <libgen.h>
#include <inttypes.h>
#include <math.h>

#define TABLE_HEADER_HEIGHT "40px"
#define TABLE_WIDTH "76%%"

#define UNCOVERED_COLOR "#ffcccc"
#define EXCLUDED_COLOR "#d6eaf8"
#define COVERED_COLOR "#ccffcc"

typedef struct _rpt_buf rpt_buf_t;

struct _rpt_buf {
   text_buf_t *tb;
   rpt_buf_t  *prev;
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
   rpt_buf_t    *rpt_buf;
} html_gen_t;

#define COV_RPT_TITLE "NVC code coverage report"

static void cover_report_hier_children(html_gen_t *g, int lvl,
                                       cover_obj_t scope, FILE *summf);
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

   fprintf(f, COV_RPT_TITLE);

   fprintf(f, "</title>\n"
           "  <style>\n");

   fputs(cov_style, f);

   fprintf(f,
              "  </style>\n"
              "</head>\n"
              "<body style=\"font-family: verdana\">\n\n");

   fprintf(f, "<header><h1 style=\"text-align: center;\">");
   fprintf(f, COV_RPT_TITLE "\n");
   fprintf(f, "</h1></header>\n\n");
}

static void cover_print_file_name(FILE *f, const rpt_file_t *src)
{
   fprintf(f, "<h2 style=\"margin-left: var(--margin-left); width: " TABLE_WIDTH ";\">\n");
   fprintf(f, "   File:&nbsp; <a href=\"../source/%s.html\">%s</a>\n",
           src ? src->path_hash : "", src ? src->path : "");
   fprintf(f, "</h2>\n\n");
}

static void cover_print_inst_name(html_gen_t *g, FILE *f, cover_obj_t scope)
{
   ident_t hier = cover_get_ident(g->data, scope, COV_ATTR_HIER);

   fprintf(f, "<h2 style=\"margin-left: var(--margin-left); width: "
           TABLE_WIDTH ";\">\n");
   fprintf(f, "   Instance:&nbsp;%s\n", istr(hier));
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
                                             const char *first_col_str)
{
   fprintf(f, "<table id=\"%s\" style=\"width: " TABLE_WIDTH ";margin-left:var(--margin-left);margin-right:auto;\"> \n"
              "  <tr style=\"height:" TABLE_HEADER_HEIGHT "\">\n"
              "    <th class=\"cbg\" onclick=\"sortTable(0, &quot;%s&quot;)\"  style=\"width:30%%\">%s</th>\n"
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

static void cover_print_item_title(html_gen_t *g, FILE *f, cover_obj_t item)
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

   switch (cover_get_kind(g->data, item)) {
   case COV_ITEM_STMT:
   case COV_ITEM_BRANCH:
   case COV_ITEM_FUNCTIONAL:
      {
         cover_src_t src = cover_get_u32(g->data, item, COV_ATTR_SOURCE,
                                         COV_SRC_UNKNOWN);
         fprintf(f, "%s", text[src]);
      }
      break;
   case COV_ITEM_EXPRESSION:
      {
         ident_t func_name = cover_get_ident(g->data, item, COV_ATTR_FUNC_NAME);
         cover_print_string(f, istr(func_name));
         fprintf(f, " expression");
      }
      break;
   case COV_ITEM_STATE:
      {
         ident_t func_name = cover_get_ident(g->data, item, COV_ATTR_FUNC_NAME);
         fprintf(f, "\"%s\" FSM", istr(func_name));
      }
      break;
   default:
      break;
   }

   const loc_t loc = cover_get_loc(g->data, item, COV_ATTR_LOC);
   if (loc.line_delta == 0)
      fprintf(f, " on line %d:", loc.first_line);
   else
      fprintf(f, " on lines %d to %d:", loc.first_line, loc.first_line + loc.line_delta);

   fprintf(f, "</h3>");
}

static void cover_print_expr(html_gen_t *g, FILE *f, cover_obj_t item,
                             cover_obj_t bin, const rpt_line_t *line)
{
   loc_t loc = cover_get_loc(g->data, item, COV_ATTR_LOC);
   cover_flags_t flags = cover_get_flags(g->data, bin);

   const rpt_line_t *curr_line = line;
   const rpt_line_t *last_line = line + loc.line_delta;
   bool was_space = false, is_expr = false;
   int lhs_beg = 0, rhs_beg = 0, lhs_end = 0, rhs_end = 0;
   int glob_pos = 0;
   bool is_comment = false;

   fprintf(f, "<code>");

   while (curr_line <= last_line) {
      int line_pos = 0;
      int line_num = loc.first_line + (curr_line - line);

      while (line_pos < curr_line->len) {

         if (curr_line == line && line_pos == loc.first_column)
            is_expr = true;

         // Track start of LHS / RHS sub-expressions
         if (flags & COVER_FLAGS_LHS_RHS_BINS) {
            loc_t loc_lhs = cover_get_loc(g->data, item, COV_ATTR_LHS_LOC);
            loc_t loc_rhs = cover_get_loc(g->data, item, COV_ATTR_RHS_LOC);

            if (loc_lhs.first_line == line_num && loc_lhs.first_column == line_pos)
               lhs_beg = glob_pos;

            if (loc_rhs.first_line == line_num && loc_rhs.first_column == line_pos)
               rhs_beg = glob_pos;

            if (loc_lhs.first_line + loc_lhs.line_delta == line_num &&
                loc_lhs.first_column + loc_lhs.column_delta == line_pos)
               lhs_end = glob_pos;

            if (loc_rhs.first_line + loc_rhs.line_delta == line_num &&
                loc_rhs.first_column + loc_rhs.column_delta == line_pos)
               rhs_end = glob_pos;
         }

         char c = curr_line->text[line_pos];

         // Filter comments
         if (line_pos < curr_line->len - 1) {
            char next_c = curr_line->text[line_pos + 1];
            if (c == '-' && next_c == '-')
               break;
            if (c == '/' && next_c == '*')
               is_comment = true;
         }

         // Multiple spaces reduced to single space
         if (!is_comment) {
            if (isspace_iso88591(c)) {
               if (!was_space) {
                  cover_print_char(f, ' ');
                  glob_pos++;
               }
               was_space = true;
            }
            else if (is_expr) {
               cover_print_char(f, c);
               glob_pos++;
               was_space = false;
            }

            if (curr_line == line + loc.line_delta &&
                line_pos == loc.first_column + loc.column_delta)
               is_expr = false;
         }

         if (line_pos > 0 && curr_line->text[line_pos - 1] == '*' && c == '/')
            is_comment = false;

         line_pos++;
      };
      curr_line++;
   }

   if (flags & COVER_FLAGS_LHS_RHS_BINS) {
      fprintf(f, "<br>");

      int lhs_mid = (lhs_end + lhs_beg) / 2;
      int rhs_mid = (rhs_end + rhs_beg) / 2;

      int curr = 0;
      while (curr < glob_pos) {
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
         else if (curr == lhs_end)
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
         else if (curr == rhs_end)
            fprintf(f, "&gt;");

         else
            fprintf(f, "&nbsp;");

         curr++;
      }
   }

   fprintf(f, "</code>");
}

static void cover_print_code_loc(html_gen_t *g, FILE *f, cover_obj_t item,
                                 const rpt_line_t *line)
{
   loc_t loc = cover_get_loc(g->data, item, COV_ATTR_LOC);
   const rpt_line_t *curr_line = line;
   const rpt_line_t *last_line = line + loc.line_delta;

   if (loc.line_delta == 0) {
      fprintf(f, "<code>");
      fprintf(f, "%d:", loc.first_line);
      cover_print_single_code_line(f, loc, curr_line);
      fprintf(f, "</code>");
   }
   else {
      fprintf(f, "<code>");

      do {
         // Shorten code samples longer than 5 lines
         if (loc.line_delta > 5 &&
             curr_line == line + 2) {
            fprintf(f, "...<br>");
            curr_line = last_line - 1;
            continue;
         }
         else
            fprintf(f, "%zu:", loc.first_line + (curr_line - line));

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

static void cover_print_get_exclude_button(html_gen_t *g, FILE *f,
                                           cover_obj_t item, cover_obj_t bin,
                                           uint32_t flag, bool add_td)
{
   if (add_td)
      fprintf(f, "<td>");

   cover_flags_t flags = cover_get_flags(g->data, bin);
   ident_t hier = cover_get_ident(g->data, bin, COV_ATTR_HIER);

   cover_item_kind_t kind = cover_get_kind(g->data, item);

   bool out_of_table = false;
   if (kind == COV_ITEM_STMT)
      out_of_table = true;
   else if ((kind == COV_ITEM_FUNCTIONAL) &&
            ((flags & COV_FLAG_USER_DEFINED) == 0))
      out_of_table = true;

   fprintf(f, "<button onclick=\"GetExclude('exclude %s')\" %s>"
           "Copy %sto Clipboard</button>", istr(hier),
           out_of_table ? "style=\"float: right;\"" : "",
           out_of_table ? "Exclude Command " : "");

   if (add_td)
      fprintf(f, "</td>");
}

static void cover_print_bin(html_gen_t *g, FILE *f, cover_obj_t item,
                            cover_obj_t bin, int32_t data, uint32_t flag,
                            cov_pair_kind_t pkind, int cols, const char **vals)
{
   cover_flags_t flags = cover_get_flags(g->data, bin);
   if (flags & flag) {
      fprintf(f, "<tr><td><b>Bin</b></td>");

      for (int i = 0; i < cols; i++)
         fprintf(f, "<td>%s</td>", vals[i]);

      uint32_t atleast = cover_get_threshold(g->data, item);

      // Toggle flags hold unreachability in highest bit of runtime data
      // Must be masked out to print properly
      fprintf(f, "<td>%d</td>", data & ~COV_FLAG_UNREACHABLE);
      fprintf(f, "<td>%d</td>", atleast);

      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(g, f, item, bin, flag, true);

      if (pkind == PAIR_EXCLUDED) {
         const char *er = ((flags | data) & COV_FLAG_UNREACHABLE)
                                                          ? "Unreachable" :
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

static void html_print_table(html_gen_t *g, const rpt_table_t *table,
                             cov_pair_kind_t pkind, FILE *f)
{
   assert(table->count > 0);

   cover_obj_t item0 = table->items[0].item;
   cover_obj_t bin0 = table->items[0].bin;

   switch (cover_get_kind(g->data, item0)) {
   case COV_ITEM_STMT:
      {
         assert(table->count == 1);

         if (pkind == PAIR_UNCOVERED)
            cover_print_get_exclude_button(g, f, item0, bin0, 0, false);
         else if (pkind == PAIR_EXCLUDED)
            fprintf(f, "<div style=\"float: right\"><b>Excluded due to:</b> Exclude file</div>");

         cover_print_item_title(g, f, item0);
         cover_print_code_loc(g, f, item0, table->line);

         fprintf(f, "<br><b>Count:</b> %d", table->items[0].data);
         fprintf(f, "<br><b>Threshold:</b> %d",
                 cover_get_threshold(g->data, item0));
      }
      break;

   case COV_ITEM_BRANCH:
      {
         cover_print_item_title(g, f, item0);
         cover_print_code_loc(g, f, item0, table->line);

         cover_flags_t flags0 = cover_get_flags(g->data, bin0);
         const char *title = (flags0 & COV_FLAG_CHOICE)
            ? "Choice of" : "Evaluated to";
         cover_print_bin_header(f, pkind, 1, &title);

         for (int i = 0; i < table->count; i++) {
            const cover_obj_t item = table->items[i].item;
            cover_obj_t bin = table->items[i].bin;
            const char *v_true = "True";
            const char *v_false = "False";

            cover_print_bin(g, f, item, bin, table->items[i].data,
                            COV_FLAG_TRUE, pkind, 1, &v_true);
            cover_print_bin(g, f, item, bin, table->items[i].data,
                            COV_FLAG_FALSE, pkind, 1, &v_false);

            if (flags0 & COV_FLAG_CHOICE) {
               loc_t loc = cover_get_loc(g->data, item, COV_ATTR_LOC);
               int curr = loc.first_column;
               int last = (loc.line_delta)
                  ? table->line->len : loc.column_delta + curr;

               LOCAL_TEXT_BUF tb = tb_new();
               tb_printf(tb, "<code>");
               while (curr <= last)
                  tb_printf(tb, "%c", table->line->text[curr++]);
               tb_printf(tb, "</code>");

               const char *v = tb_get(tb);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_CHOICE,
                               pkind, 1, &v);
            }
         }

         fprintf(f, "</table>");
      }
      break;

   case COV_ITEM_EXPRESSION:
      {
         cover_print_item_title(g, f, item0);
         cover_print_expr(g, f, item0, bin0, table->line);

         cover_flags_t flags0 = cover_get_flags(g->data, bin0);

         if (flags0 & (COV_FLAG_TRUE | COV_FLAG_FALSE)) {
            const char *title = "Evaluated to";
            cover_print_bin_header(f, pkind, 1, &title);
         }
         else {
            const char *title[2] = { "LHS", "RHS" };
            cover_print_bin_header(f, pkind, 2, title);
         }

         for (int i = 0; i < table->count; i++) {
            cover_obj_t item = table->items[i].item;
            cover_obj_t bin = table->items[i].bin;
            cover_flags_t flags = cover_get_flags(g->data, bin);

            const char *t_str = (flags & COV_FLAG_EXPR_STD_LOGIC)
               ? "'1'" : "True";
            const char *f_str = (flags & COV_FLAG_EXPR_STD_LOGIC)
               ? "'0'" : "False";

            const char *ff[2] = {f_str, f_str};
            const char *ft[2] = {f_str, t_str};
            const char *tf[2] = {t_str, f_str};
            const char *tt[2] = {t_str, t_str};

            if (flags & (COV_FLAG_TRUE | COV_FLAG_FALSE)) {
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_TRUE,
                               pkind, 1, &t_str);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_FALSE,
                               pkind, 1, &f_str);
            }
            else if (flags & (COV_FLAG_00 | COV_FLAG_01
                              | COV_FLAG_10 | COV_FLAG_11)) {
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_00, pkind, 2, ff);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_01, pkind, 2, ft);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_10, pkind, 2, tf);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_11, pkind, 2, tt);
            }
         }

         fprintf(f, "</table>");
      }
      break;

   case COV_ITEM_TOGGLE:
      {
         assert(table->count > 0);

         cover_flags_t flags0 = cover_get_flags(g->data, bin0);
         ident_t hier0 = cover_get_ident(g->data, bin0, COV_ATTR_HIER);

         if (flags0 & COV_FLAG_TOGGLE_SIGNAL)
            fprintf(f, "<h3>Signal:</h3>");
         else if (flags0 & COV_FLAG_TOGGLE_PORT)
            fprintf(f, "<h3>Port:</h3>");

         int64_t metadata0 =
            cover_get_i64(g->data, item0, COV_ATTR_METADATA, 0);
         const char *sig_name = istr(hier0) + metadata0;

         const char *bin_name = strrchr(sig_name, '.');
         assert(bin_name != NULL);

         int name_len = 0;
         while (sig_name[name_len] != '\0' && sig_name[name_len] != '.'
                && sig_name[name_len] != '(')
            name_len++;

         fprintf(f, "&nbsp;<code>%.*s</code>", name_len, sig_name);

         if (sig_name[name_len] == '.')
            name_len++;

         const bool is_composite = sig_name + name_len < bin_name;

         if (is_composite) {
            const char *title[3] = { "Element", "From", "To" };
            cover_print_bin_header(f, pkind, 3, title);

            LOCAL_TEXT_BUF tb = tb_new();

            for (int i = 0; i < table->count; i++) {
               cover_obj_t item = table->items[i].item;
               cover_obj_t bin = table->items[i].bin;
               ident_t hier = cover_get_ident(g->data, bin, COV_ATTR_HIER);
               const char *elem_name = istr(hier) + metadata0 + name_len;

               const char *bin_name = strrchr(elem_name, '.');
               assert(bin_name != NULL);

               tb_rewind(tb);
               tb_catn(tb, elem_name, bin_name - elem_name);

               const char *v_01[3] = { tb_get(tb), "0", "1" };
               const char *v_10[3] = { tb_get(tb), "1", "0" };
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_TOGGLE_TO_1,
                               pkind, 3, v_01);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_TOGGLE_TO_0,
                               pkind, 3, v_10);
            }
         }
         else {
            const char *title[2] = { "From", "To" };
            cover_print_bin_header(f, pkind, 2, title);

            for (int i = 0; i < table->count; i++) {
               cover_obj_t item = table->items[i].item;
               cover_obj_t bin = table->items[i].bin;
               const char *v_01[2] = { "0", "1" };
               const char *v_10[2] = { "1", "0" };
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_TOGGLE_TO_1,
                               pkind, 2, v_01);
               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_TOGGLE_TO_0,
                               pkind, 2, v_10);
            }
         }

         fprintf(f, "</table>");
      }
      break;

   case COV_ITEM_FUNCTIONAL:
      {
         if (cover_get_u32(g->data, item0, COV_ATTR_SOURCE,
                           COV_SRC_UNKNOWN) == COV_SRC_USER_COVER) {
            cover_print_item_title(g, f, item0);
            ident_t func_name =
               cover_get_ident(g->data, item0, COV_ATTR_FUNC_NAME);
            fprintf(f, "<br>%s", istr(func_name));

            const int n_ranges0 = cover_count(g->data, bin0, COV_REL_RANGES);

            const char *title[n_ranges0];
            for (int i = 0; i < n_ranges0; i++)
               title[i] = xasprintf("Variable %d", i);

            cover_print_bin_header(f, pkind, n_ranges0, title);

            for (int i = 0; i < table->count; i++) {
               cover_obj_t item = table->items[i].item;
               cover_obj_t bin = table->items[i].bin;
               assert(cover_get_u32(g->data, item, COV_ATTR_SOURCE,
                                    COV_SRC_UNKNOWN) == COV_SRC_USER_COVER);

               const int n_ranges = cover_count(g->data, bin, COV_REL_RANGES);

               const char *v[n_ranges];
               for (int j = 0; j < n_ranges; j++) {
                  cover_obj_t r = cover_at(g->data, bin, COV_REL_RANGES, j);

                  int64_t max = cover_get_i64(g->data, r, COV_ATTR_MAX, 0);
                  int64_t min = cover_get_i64(g->data, r, COV_ATTR_MIN, 0);

                  if (min == max)
                     v[j] = xasprintf("%"PRIi64, min);
                  else
                     v[j] = xasprintf("%"PRIi64" - %"PRIi64, min, max);
               }

               cover_print_bin(g, f, item, bin, table->items[i].data,
                               COV_FLAG_USER_DEFINED, pkind, n_ranges, v);
            }

            fprintf(f, "</table>");
         }
         else {
            if (pkind == PAIR_UNCOVERED)
               cover_print_get_exclude_button(g, f, item0, bin0, 0, false);
            cover_print_item_title(g, f, item0);
            cover_print_code_loc(g, f, item0, table->line);
            fprintf(f, "<br><b>Count:</b> %d", table->items[0].data);
            fprintf(f, "<br><b>Threshold:</b> %d",
                    cover_get_threshold(g->data, item0));
         }
      }
      break;

   case COV_ITEM_STATE:
      {
         cover_print_item_title(g, f, item0);
         cover_print_code_loc(g, f, item0, table->line);

         const char *title = "State";
         cover_print_bin_header(f, pkind, 1, &title);

         for (int i = 0; i < table->count; i++) {
            cover_obj_t item = table->items[i].item;
            cover_obj_t bin = table->items[i].bin;
            ident_t hier = cover_get_ident(g->data, bin, COV_ATTR_HIER);
            ident_t state_name = ident_rfrom(hier, '.');
            const char *v = istr(state_name);
            cover_print_bin(g, f, item, bin, table->items[i].data,
                            COV_FLAG_STATE, pkind, 1, &v);
         }

         fprintf(f, "</table>");
      }
      break;
   }
}

static void html_print_detail(html_gen_t *g, const rpt_detail_t *d,
                              cover_item_kind_t kind, FILE *f)
{
   static const char *div_id[] = {
      [COV_ITEM_STMT] = "Statement",
      [COV_ITEM_BRANCH] = "Branch",
      [COV_ITEM_TOGGLE] = "Toggle",
      [COV_ITEM_EXPRESSION] = "Expression",
      [COV_ITEM_STATE] = "FSM_state",
      [COV_ITEM_FUNCTIONAL] = "Functional",
   };

   static const char *title[] = {
      [COV_ITEM_STMT] = "statements",
      [COV_ITEM_BRANCH] = "branches",
      [COV_ITEM_TOGGLE] = "toggles",
      [COV_ITEM_EXPRESSION] = "expressions",
      [COV_ITEM_STATE] = "FSM states",
      [COV_ITEM_FUNCTIONAL] = "functional coverage",
   };

   fprintf(f, "<div id=\"%s\" class=\"tabcontent\" style=\"width:" TABLE_WIDTH ";"
           "margin-left:var(--margin-left); "
           "margin-right:auto; margin-top:-2px; "
           "border: 2px solid black;\">\n", div_id[kind]);

   if (!cover_enabled(g->data, COVER_MASK_DONT_PRINT_UNCOVERED)) {
      fprintf(f, "  <section style=\"padding-left:10px; padding-top: 10px; "
              "padding-bottom: 10px; padding-right:10px;"
              "background-color:"UNCOVERED_COLOR ";\">\n");

      fprintf(f, " <h2 style=\"margin-top: 0px; margin-bottom: 0px\">Uncovered %s:</h2>\n",
              title[kind]);

      fprintf(f, "  <div style=\"padding:0px 10px;\">\n");
      for (int i = 0; i < d->miss[kind].count; i++) {
         if (i > 0) fprintf(f, "<hr/>\n");
         html_print_table(g, d->miss[kind].items[i], PAIR_UNCOVERED, f);
      }
      fprintf(f, "  </div>\n");

      fprintf(f, "  </section>\n\n");
   }

   if (!cover_enabled(g->data, COVER_MASK_DONT_PRINT_EXCLUDED)) {
      fprintf(f, "  <section style=\"padding-left:10px; padding-top: 10px; "
              "padding-bottom: 10px; padding-right:10px;"
              "background-color:"EXCLUDED_COLOR ";\">\n");

      fprintf(f, " <h2 style=\"margin-top: 0px; margin-bottom: 0px\">Excluded %s:</h2>\n",
              title[kind]);

      fprintf(f, "  <div style=\"padding:0px 10px;\">\n");
      for (int i = 0; i < d->excl[kind].count; i++) {
         if (i > 0) fprintf(f, "<hr/>\n");
         html_print_table(g, d->excl[kind].items[i], PAIR_EXCLUDED, f);
      }
      fprintf(f, "  </div>\n");

      fprintf(f, "  </section>\n\n");
   }

   if (!cover_enabled(g->data, COVER_MASK_DONT_PRINT_COVERED)) {
      fprintf(f, "  <section style=\"padding-left:10px; padding-top: 10px; "
              "padding-bottom: 10px; padding-right:10px;"
              "background-color:"COVERED_COLOR ";\">\n");

      fprintf(f, " <h2 style=\"margin-top: 0px; margin-bottom: 0px\">Covered %s:</h2>\n",
              title[kind]);

      fprintf(f, "  <div style=\"padding:0px 10px;\">\n");
      for (int i = 0; i < d->hits[kind].count; i++) {
         if (i > 0) fprintf(f, "<hr/>\n");
         html_print_table(g, d->hits[kind].items[i], PAIR_COVERED, f);
      }
      fprintf(f, "  </div>\n");

      fprintf(f, "  </section>\n\n");
   }

   fprintf(f, "</div>\n");
}

static void html_print_tabs(FILE *f)
{
   fprintf(f,
           "<table style=\"width:" TABLE_WIDTH ";margin-left:var(--margin-left);margin-right:auto;\"> \n"
           "   <tr style=\"height:" TABLE_HEADER_HEIGHT "\">\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</th>\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'Branch')\">Branch</th>\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</th>\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'Expression')\">Expression</th>\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'FSM_state')\">FSM state</th>\n"
           "      <th class=\"cbg\" onclick=\"selectCoverage(event, 'Functional')\">Functional</th>\n"
           "   </tr>\n"
           "</table>\n\n");
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

static void cover_print_summary_table_row(html_gen_t *g, FILE *f, const rpt_stats_t *stats,
                                          ident_t entry_name, ident_t entry_link, int lvl,
                                          bool top, bool print_out)
{
   fprintf(f, "  <tr>\n"
              "    <td style=\"background-color:var(--table-row-color)\">\n"
              "<a href=\"%s%s.html\">%s</a></td>\n",
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

      rpt_buf_t *new = xcalloc(sizeof(rpt_buf_t));
      new->tb = tb_new();
      new->prev = g->rpt_buf;
      g->rpt_buf = new;

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

static void cover_print_nav_hier_node(html_gen_t *g, FILE *f, cover_obj_t scope,
                                      cover_obj_t sel)
{
   const char *link = rpt_get_hier(g->rpt, scope)->name_hash;

   bool open = false;
   for (cover_obj_t it = sel; !open && !cover_is_null(it);) {
      open |= cover_equals(it, scope);

      cover_obj_t parent = cover_get_obj(g->data, it, COV_ATTR_PARENT);
      if (cover_is_null(parent))
         break;

      it = parent;
   }

   const bool leaf = cover_is_leaf(g->data, scope);
   if (!leaf) {
      fprintf(f, "<details%s>\n", open ? " open" : "");
      fprintf(f, "<summary>");
   }

   ident_t name = cover_get_ident(g->data, scope, COV_ATTR_NAME);

   fprintf(f, "<a href=\"%s.html\"", link);
   if (cover_equals(scope, sel))
      fprintf(f, " class=\"nav-sel\"");
   fprintf(f, ">%s</a>\n", istr(name));

   if (!leaf) {
      fprintf(f, "</summary>\n");

      const int nchildren = cover_count(g->data, scope, COV_REL_CHILDREN);

      for (int i = 0; i < nchildren; i++) {
         cover_obj_t child = cover_at(g->data, scope, COV_REL_CHILDREN, i);
         if (cover_is_hier(g->data, child))
            cover_print_nav_hier_node(g, f, child, sel);
      }

      fprintf(f, "</details>\n");
   }
}

static void cover_print_hier_nav_tree(html_gen_t *g, FILE *f, cover_obj_t scope)
{
   fprintf(f, "<h2 style=\"float: left; margin-top: 50px; \">Hierarchy</h2>\n");

   ident_t name = cover_get_ident(g->data, g->data->root_scope, COV_ATTR_NAME);

   fprintf(f, "<nav style=\"clear: left\">\n");
   fprintf(f, "<details open>\n");
   fprintf(f, "<summary><a href=\"../index.html\">%s</a></summary>\n",
           istr(name));

   const int nchildren = cover_count(g->data, g->data->root_scope,
                                     COV_REL_CHILDREN);

   for (int i = 0; i < nchildren; i++) {
      cover_obj_t child = cover_at(g->data, g->data->root_scope,
                                   COV_REL_CHILDREN, i);
      cover_print_nav_hier_node(g, f, child, scope);
   }

   fprintf(f, "</details>\n");
   fprintf(f, "</nav>\n\n");
}

static void cover_report_hier(html_gen_t *g, int lvl, cover_obj_t scope)
{
   const rpt_hier_t *h = rpt_get_hier(g->rpt, scope);

   FILE *f = create_file("%s/hier/%s.html", g->outdir, h->name_hash);

   cover_print_html_header(f);
   cover_print_hier_nav_tree(g, f, scope);
   cover_print_inst_name(g, f, scope);

   const rpt_file_t *src = rpt_get_file(g->rpt, scope);
   cover_print_file_name(f, src);

   if (!cover_is_leaf(g->data, scope)) {
      cover_print_summary_table_header(f, "sub_inst_table", "Nested Instances");

      cover_report_hier_children(g, lvl, scope, f);

      cover_print_table_footer(f);
   }

   fprintf(f, "<br>");

   cover_print_summary_table_header(f, "cur_inst_table", "Current Instance");

   ident_t hier = cover_get_ident(g->data, scope, COV_ATTR_HIER);
   ident_t rpt_name_id = ident_new(h->name_hash);
   cover_print_summary_table_row(g, f, &(h->flat_stats), hier,
                                 rpt_name_id, lvl, false, false);
   cover_print_table_footer(f);

   fprintf(f, "<h2 style=\"margin-left: var(--margin-left);\">\n  Details:\n</h2>\n\n");

   const int skipped = rpt_get_skipped(g->rpt);
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: var(--margin-left);\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h3><br>\n\n", g->item_limit, skipped);

   html_print_tabs(f);

   for (cover_item_kind_t kind = 0; kind < NUM_COVER_KINDS; kind++)
      html_print_detail(g, &h->detail, kind, f);

   cover_print_jscript_funcs(f);
   cover_print_timestamp(f);

   fclose(f);
}

static void cover_report_hier_children(html_gen_t *g, int lvl,
                                       cover_obj_t scope, FILE *summf)
{
   cover_iter_t it = cover_begin(g->data, scope, COV_REL_CHILDREN);
   cover_obj_t child;
   while (cover_next(&it, &child)) {
      if (cover_is_hier(g->data, child)) {
         cover_report_hier(g, lvl + 2, child);

         const rpt_hier_t *h = rpt_get_hier(g->rpt, child);

         ident_t hier = cover_get_ident(g->data, child, COV_ATTR_HIER);

         cover_print_summary_table_row(g, summf, &(h->nested_stats),
                                       ident_rfrom(hier, '.'),
                                       ident_new(h->name_hash),
                                       lvl + 2, false, true);
      }
      else
         cover_report_hier_children(g, lvl, child, summf);
   }
}

static void cover_report_per_hier(html_gen_t *g, FILE *f, cover_rpt_t *rpt)
{
   const int nchildren = cover_count(g->data, g->data->root_scope,
                                     COV_REL_CHILDREN);

   for (int i = 0; i < nchildren; i++) {
      cover_obj_t child = cover_at(g->data, g->data->root_scope,
                                   COV_REL_CHILDREN, i);

      cover_report_hier(g, 0, child);

      ident_t hier = cover_get_ident(g->data, child, COV_ATTR_HIER);

      const rpt_hier_t *h = rpt_get_hier(rpt, child);
      cover_print_summary_table_row(g, f, &(h->nested_stats), hier,
                                    ident_new(h->name_hash), 0, true, true);
   }

   if (opt_get_int(OPT_VERBOSE)) {
      notef("Coverage for sub-hierarchies:");
      printf("%-65s %-30s %-30s %-30s %-30s %-30s %-30s %-30s\n",
             "Hierarchy", "Statement", "Branch", "Toggle", "Expression",
             "FSM state", "Functional", "Average");
      rpt_buf_t *buf = g->rpt_buf;
      while (buf) {
         printf("%s\n", tb_get(buf->tb));
         tb_free(buf->tb);
         g->rpt_buf = buf->prev;
         free(buf);
         buf = g->rpt_buf;
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
   fprintf(f, "<h2 style=\"float: left; margin-bottom: 0px; margin-top: 0px;\"><a href=../index.html>Back to summary</a></h2>\n");
   fprintf(f, "<h2 style=\"float: left; clear: left;\">Coverage report for file:</h2>\n");

   fprintf(f, "<nav style=\"clear: left\">\n");

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

static void cover_report_per_file(html_gen_t *g, FILE *top_f, cover_rpt_t *rpt)
{
   const int n_files = rpt_iter_files(rpt, NULL, NULL);
   const rpt_file_t **files LOCAL =
      xmalloc_array(n_files, sizeof(rpt_file_t *)), **p = files;

   rpt_iter_files(rpt, cover_store_file_cb, &p);
   assert(p == files + n_files);

   qsort(files, n_files, sizeof(rpt_file_t *), cover_sort_files_cb);

   for (int i = 0; i < n_files; i++) {
      // Print per-file report
      char *file_name LOCAL = xstrdup(files[i]->path);
      ident_t base_name_id = ident_new(basename(file_name));

      FILE *f = create_file("%s/hier/%s.html", g->outdir, istr(base_name_id));

      cover_print_html_header(f);
      cover_print_file_nav_tree(f, n_files, files);
      cover_print_file_name(f, files[i]);

      fprintf(f, "<h2 style=\"margin-left: var(--margin-left);\">\n  Current File:\n</h2>\n\n");
      cover_print_summary_table_header(f, "cur_file_table", "File");
      cover_print_summary_table_row(g, f, &(files[i]->stats), base_name_id,
                                    base_name_id, 0, false, false);
      cover_print_table_footer(f);

      fprintf(f, "<h2 style=\"margin-left: var(--margin-left);\">\n  Details:\n</h2>\n\n");

      const int skipped = rpt_get_skipped(rpt);
      if (skipped)
         fprintf(f, "<h3 style=\"margin-left: var(--margin-left);\">The limit of "
                    "printed items was reached (%d). Total %d items are not "
                    "displayed.</h3>\n\n", g->item_limit, skipped);

      html_print_tabs(f);

      for (cover_item_kind_t kind = 0; kind < NUM_COVER_KINDS; kind++)
         html_print_detail(g, &(files[i]->detail), kind, f);

      cover_print_jscript_funcs(f);

      cover_print_timestamp(f);

      // Print top table summary
      cover_print_summary_table_row(g, top_f, &(files[i]->stats),
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
      cover_print_summary_table_header(f, "file_table", "File");
      cover_report_per_file(&g, f, rpt);
   }
   else {
      cover_print_summary_table_header(f, "inst_table", "Current Instance");
      cover_report_per_hier(&g, f, rpt);
   }

   cover_print_timestamp(f);

   cover_report_free(rpt);
   fclose(f);
}
