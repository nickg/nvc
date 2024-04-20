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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

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
   int flags;
} cover_pair_t;

typedef struct {
   cover_pair_t *hits;
   cover_pair_t *miss;
   cover_pair_t *excl;
   int          n_hits;
   int          n_miss;
   int          n_excl;
   int          alloc_hits;
   int          alloc_miss;
   int          alloc_excl;
} cover_chain_t;

typedef struct {
   const char   *name;
   cover_line_t *lines;
   unsigned      n_lines;
   unsigned      alloc_lines;
   bool          valid;
} cover_file_t;

struct _cover_report_ctx {
   cover_data_t         *data;
   cover_stats_t        flat_stats;
   cover_stats_t        nested_stats;
   cover_report_ctx_t   *parent;
   cover_chain_t        ch_stmt;
   cover_chain_t        ch_branch;
   cover_chain_t        ch_toggle;
   cover_chain_t        ch_expression;
   cover_chain_t        ch_state;
   cover_chain_t        ch_functional;
   int                  lvl;
};

typedef enum {
   PAIR_UNCOVERED    = 0,
   PAIR_EXCLUDED     = 1,
   PAIR_COVERED      = 2,
   PAIR_LAST         = 3
} cov_pair_kind_t;

#define INIT_CHAIN(ctx, name)                                           \
   ctx->name.hits = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.miss = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.excl = xcalloc_array(1024, sizeof(cover_pair_t));          \
   ctx->name.alloc_hits = 1024;                                         \
   ctx->name.alloc_miss = 1024;                                         \
   ctx->name.alloc_excl = 1024;                                         \

#define COV_RPT_TITLE "NVC code coverage report"

static void cover_report_children(cover_report_ctx_t *ctx,
                                  cover_scope_t *s, const char *dir,
                                  FILE *summf, int *skipped);

///////////////////////////////////////////////////////////////////////////////
// Report generation
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

   static shash_t *files = NULL;

   if (files == NULL)
      files = shash_new(64);

   const char *name = loc_file_str(&(s->loc));
   cover_file_t *f = shash_get(files, name);

   if (f != NULL)
      return f->valid ? f : NULL;

   f = xmalloc(sizeof(cover_file_t));
   f->name        = name;
   f->n_lines     = 0;
   f->alloc_lines = 1024;
   f->lines       = xmalloc_array(f->alloc_lines, sizeof(cover_line_t));
   f->valid       = false;

   shash_put(files, name, f);

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
              "<html>\n"
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
              "   nav {\n"
              "      float: left;\n"
              "      background-color: #ccc;\n"
              "      width: " SIDEBAR_WIDTH ";\n"
              "      height: 100%%;\n"
              "      padding: 10px;\n"
              "      margin-top: 100px;\n"
              "      word-wrap: break-word;\n"
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
              "     background-color: none;\n"
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
              "     margin-left: 20px\n"
              "     border: 2px solid black;\n"
              "     cursor: pointer;\n"
              "     padding: 14px 16px;\n"
              "     font-size: 17px;\n"
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
              "  </style>\n"
              "  </head>\n"
              "  <section>\n\n");

   fprintf(f, "<header>");
   fprintf(f, COV_RPT_TITLE "\n");
   fprintf(f, "</header>\n\n");
}

static void cover_print_navigation_tree(FILE *f, cover_report_ctx_t *ctx,
                                        cover_scope_t *s)
{
   fprintf(f, "<nav>\n"
               "<b>Hierarchy:</b><br>\n");
   int offset = 0;

   ident_t full_hier = s->hier;
   ident_t curr_id;
   ident_t curr_hier = NULL;
   const char *link = "../index";

   do {
      curr_id = ident_walk_selected(&full_hier);
      curr_hier = ident_prefix(curr_hier, curr_id, '.');
      if (offset > 0)
         link = istr(curr_hier);
      if (curr_id)
         fprintf(f, "<p style=\"margin-left: %dpx\"><a href=%s.html>%s</a></p>\n",
                     offset * 10, link, istr(curr_id));
      offset++;
   } while (curr_id != NULL);

   fprintf(f, "</nav>\n\n");

}

static void cover_print_file_and_inst(FILE *f, cover_report_ctx_t *ctx,
                                      cover_scope_t *s)
{
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   fprintf(f, "   Instance:&nbsp;%s\n", istr(s->hier));
   fprintf(f, "</h2>\n\n");

   cover_file_t *src = cover_file_for_scope(s);
   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n");
   if (src != NULL)
      fprintf(f, "   File:&nbsp; <a href=\"../../%s\">../../%s</a>\n",
                  src->name, src->name);
   fprintf(f, "</h2>\n\n");
}


static void cover_print_percents_cell(FILE *f, unsigned hit, unsigned total)
{
   if (total > 0) {
      float perc = ((float) hit / (float) total) * 100;
      char color[8];
      if (hit == total)
         checked_sprintf(color, sizeof(color), "#00cc00");
      else if (perc > 90)
         checked_sprintf(color, sizeof(color), "#e6e600");
      else if (perc > 80)
         checked_sprintf(color, sizeof(color), "#ff9900");
      else
         checked_sprintf(color, sizeof(color), "#ff0000");

      fprintf(f, "    <td bgcolor=%s>%.1f %% (%d/%d)</td>\n",
              color, perc, hit, total);
      return;
   }

   fprintf(f, "    <td bgcolor=#aaaaaa>N.A.</td>\n");
}

static void cover_print_hierarchy_header(FILE *f)
{
   fprintf(f, "<table style=\"width:75%%;margin-left:" MARGIN_LEFT ";margin-right:auto;\"> \n"
              "  <tr>\n"
              "    <th class=\"cbg\" style=\"width:30%%\">Instance</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Statement</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Branch</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Toggle</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Expression</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">FSM state</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Functional</th>\n"
              "    <th class=\"cbg\" style=\"width:8%%\">Average</th>\n"
              "  </tr>\n");
}

static void cover_print_hierarchy_footer(FILE *f)
{
   fprintf(f, "</table>\n\n");
}

static void cover_print_timestamp(FILE *f)
{
   time_t t;
   time(&t);

   fprintf(f, "  </section>\n");

   fprintf(f, "<footer>");
   fprintf(f, "   <p> NVC version: %s </p>\n", PACKAGE_VERSION);
   fprintf(f, "   <p> Generated on: %s </p>\n", ctime(&t));
   fprintf(f,  "</footer>");
}

static void cover_print_hierarchy_summary(FILE *f, cover_report_ctx_t *ctx, ident_t hier,
                                          bool top, bool full_hier, bool flat)
{
   ident_t print_hier = (full_hier) ? hier : ident_rfrom(hier, '.');
   cover_stats_t *stats;

   if (flat)
      stats = &(ctx->flat_stats);
   else
      stats = &(ctx->nested_stats);

   fprintf(f, "  <tr>\n"
              "    <td><a href=\"%s%s.html\">%s</a></td>\n",
              top ? "hier/" : "", istr(hier), istr(print_hier));

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
      notef("code coverage results for: %s", istr(hier));

      if (perc_stmt > 0)
         notef("     statement:     %.1f %% (%d/%d)", perc_stmt,
               stats->hit_stmts, stats->total_stmts);
      else
         notef("     statement:     N.A.");

      if (perc_branch > 0)
         notef("     branch:        %.1f %% (%d/%d)", perc_branch,
               stats->hit_branches, stats->total_branches);
      else
         notef("     branch:        N.A.");

      if (perc_toggle > 0)
         notef("     toggle:        %.1f %% (%d/%d)", perc_toggle,
               stats->hit_toggles, stats->total_toggles);
      else
         notef("     toggle:        N.A.");

      if (perc_expr > 0)
         notef("     expression:    %.1f %% (%d/%d)", perc_expr,
               stats->hit_expressions, stats->total_expressions);
      else
         notef("     expression:    N.A.");

      if (perc_state > 0)
         notef("     FSM state:     %.1f %% (%d/%d)", perc_state,
               stats->hit_states, stats->total_states);
      else
         notef("     FSM state:     N.A.");

      if (perc_functional > 0)
         notef("     functional:    %.1f %% (%d/%d)", perc_functional,
               stats->hit_functional, stats->total_functional);
      else
         notef("     functional:    N.A.");
   }
   else if (opt_get_int(OPT_VERBOSE) && !flat) {

      cover_rpt_buf_t *new = xcalloc(sizeof(cover_rpt_buf_t));
      new->tb = tb_new();
      new->prev = ctx->data->rpt_buf;
      ctx->data->rpt_buf = new;

      tb_printf(new->tb,
         "%*s %-*s %10.1f %% (%d/%d)  %10.1f %% (%d/%d) %10.1f %% (%d/%d) "
         "%10.1f %% (%d/%d) %10.1f %% (%d/%d)",
         ctx->lvl, "", 50-ctx->lvl, istr(ident_rfrom(hier, '.')),
         perc_stmt, stats->hit_stmts, stats->total_stmts,
         perc_branch, stats->hit_branches, stats->total_branches,
         perc_toggle, stats->hit_toggles, stats->total_toggles,
         perc_expr, stats->hit_expressions, stats->total_expressions,
         perc_state, stats->hit_states, stats->total_states);
   }
}

static inline void cover_print_char(FILE *f, char c)
{
   if (c == '\n' || c == ' ')
      fprintf(f, "&nbsp");
   else
      fprintf(f, "%c", c);
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
      [COV_SRC_UNKNOWN] = "",
   };

   fprintf(f, "<h3>");

   switch (pair->item->kind) {
   case COV_ITEM_STMT:
   case COV_ITEM_BRANCH:
      fprintf(f, "%s:", text[pair->item->source]);
      break;
   case COV_ITEM_EXPRESSION:
      fprintf(f, "%s expression", istr(pair->item->func_name));
      break;
   case COV_ITEM_STATE:
      fprintf(f, "\"%s\" FSM with total %d states", istr(pair->item->func_name),
              pair->item->num);
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

   cover_print_item_title(f, pair);

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
            fprintf(f, "%lu:", loc.first_line + (curr_line - pair->line));

         int curr_char = 0;
         while (curr_char < curr_line->len) {
            cover_print_char(f, curr_line->text[curr_char]);
            curr_char++;
         }

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

   ident_t hier = item->hier;
   LOCAL_TEXT_BUF tb = tb_new();

   // State coverage contains bin name (state name) appended to hierarchical path
   if (item->kind == COV_ITEM_STATE) {
      tb_istr(tb, ident_rfrom(hier, '.'));
      hier = ident_runtil(hier, '.');
   }
   else if (flag)
      cover_bmask_to_bin_list(flag, tb);

   bool out_of_table = (item->kind == COV_ITEM_STMT) || (item->kind == COV_ITEM_FUNCTIONAL);
   fprintf(f, "<button onclick=\"GetExclude('exclude %s %s')\" %s>"
           "Copy %sto Clipoard</button>", istr(hier), tb_get(tb),
           out_of_table ? "style=\"float: right;\"" : "",
           out_of_table ? "Exclude Command " : "");

   if (add_td)
      fprintf(f, "</td>");
}

// TODO: Remove once all coverage item types are ported to counters!
static void cover_print_bin_legacy(FILE *f, cover_pair_t *pair, uint32_t flag,
                                   cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   if (pair->item->flags & flag) {
      fprintf(f, "<tr><td><b>Bin</b></td>");

      for (int i = 0; i < cols; i++) {
         const char *val = va_arg(argp, const char *);
         fprintf(f, "<td>%s</td>", val);
      }

      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(f, pair->item, flag, true);

      if (pkind == PAIR_EXCLUDED) {
         const char *er = (flag & pair->item->unrc_msk) ?
            "Unreachable" : "Exclude file";
         fprintf(f, "<td>%s</td>", er);
      }

      fprintf(f, "</tr>");
   }
}

static void cover_print_bin(FILE *f, cover_pair_t *pair, uint32_t flag,
                            cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   if (pair->item->flags & flag) {
      fprintf(f, "<tr><td><b>Bin</b></td>");

      for (int i = 0; i < cols; i++) {
         const char *val = va_arg(argp, const char *);
         fprintf(f, "<td>%s</td>", val);
      }

      if (pkind == PAIR_UNCOVERED)
         cover_print_get_exclude_button(f, pair->item, flag, true);

      if (pkind == PAIR_EXCLUDED) {
         const char *er = (pair->item->flags & COV_FLAG_UNREACHABLE) ?
            "Unreachable" : "Exclude file";
         fprintf(f, "<td>%s</td>", er);
      }

      fprintf(f, "</tr>");
   }
}

// TODO: Remove once all cover_item kinds
static void cover_print_bin_header_legacy(FILE *f, cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   fprintf(f, "<tr><th></th>");

   for (int i = 0; i < cols; i++) {
      const char *val = va_arg(argp, const char *);
      fprintf(f, "<th>%s</th>", val);
   }

   if (pkind == PAIR_UNCOVERED)
      fprintf(f, "<th>Exclude Command</th>");

   if (pkind == PAIR_EXCLUDED)
      fprintf(f, "<th>Excluded due to</th>");

   fprintf(f, "</tr>");
}

static void cover_print_bin_header(FILE *f, cov_pair_kind_t pkind, int cols, ...)
{
   va_list argp;
   va_start(argp, cols);

   fprintf(f, "<br><table class=\"cbt\">");
   fprintf(f, "<tr><th></th>");

   for (int i = 0; i < cols; i++) {
      const char *val = va_arg(argp, const char *);
      fprintf(f, "<th>%s</th>", val);
   }

   if (pkind == PAIR_UNCOVERED)
      fprintf(f, "<th>Exclude Command</th>");

   if (pkind == PAIR_EXCLUDED)
      fprintf(f, "<th>Excluded due to</th>");

   fprintf(f, "</tr>");
}

// TODO: Remove once all item kinds indicate items to be grouped into single table via
//       "num" attribute.
static void cover_print_bins_legacy(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind)
{
   loc_t loc = pair->item->loc;

   fprintf(f, "<br><table class=\"cbt\">");

   switch (pair->item->kind) {
   case COV_ITEM_BRANCH:
      cover_print_bin_header_legacy(f, pkind, 1, (pair->flags & COV_FLAG_CHOICE) ?
                                    "Choice of" : "Evaluated to");
      cover_print_bin_legacy(f, pair, COV_FLAG_TRUE, pkind, 1, "True");
      cover_print_bin_legacy(f, pair, COV_FLAG_FALSE, pkind, 1, "False");

      if (pair->flags & COV_FLAG_CHOICE) {
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

         cover_print_bin_legacy(f, pair, COV_FLAG_CHOICE, pkind, 1, tb_get(tb));
      }
      break;

   case COV_ITEM_EXPRESSION:
      {
      char *t_str = (pair->item->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'1'" : "True";
      char *f_str = (pair->item->flags & COV_FLAG_EXPR_STD_LOGIC) ? "'0'" : "False";

      if ((pair->flags & COV_FLAG_TRUE) || (pair->flags & COV_FLAG_FALSE)) {
         cover_print_bin_header_legacy(f, pkind, 1, "Evaluated to");
         cover_print_bin_legacy(f, pair, COV_FLAG_TRUE, pkind, 1, t_str);
         cover_print_bin_legacy(f, pair, COV_FLAG_FALSE, pkind, 1, f_str);
      }

      if (pair->flags & COV_FLAG_00 || pair->flags & COV_FLAG_01 ||
          pair->flags & COV_FLAG_10 || pair->flags & COV_FLAG_11) {
         cover_print_bin_header_legacy(f, pkind, 2, "LHS", "RHS");

         cover_print_bin_legacy(f, pair, COV_FLAG_00, pkind, 2, f_str, f_str);
         cover_print_bin_legacy(f, pair, COV_FLAG_01, pkind, 2, f_str, t_str);
         cover_print_bin_legacy(f, pair, COV_FLAG_10, pkind, 2, t_str, f_str);
         cover_print_bin_legacy(f, pair, COV_FLAG_11, pkind, 2, t_str, t_str);
      }
      }
      break;
   default:
      fatal("unsupported type of code coverage: %d !", pair->item->kind);
   }
   fprintf(f, "</table>");
}


static void cover_print_bins(FILE *f, cover_pair_t *first_pair, cov_pair_kind_t pkind)
{
   cover_pair_t *last_pair = first_pair + first_pair->item->num - 1;
   for (cover_pair_t *pair = first_pair; pair <= last_pair; pair++)
   {
      switch (pair->item->kind) {
      case COV_ITEM_TOGGLE:
         cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_1, pkind, 2, "0", "1");
         cover_print_bin(f, pair, COV_FLAG_TOGGLE_TO_0, pkind, 2, "1", "0");
         break;

      default:
         fatal("unsupported type of code coverage: %d !", pair->item->kind);
      }
   }

   fprintf(f, "</table>");
}


static int cover_print_fsm_table(FILE *f, cover_pair_t *pair, cov_pair_kind_t pkind,
                                 cover_pair_t *last)
{
   // All pairs of a single FSM are consequent. Group them together and print them
   // in a single table.
   ident_t first_prefix = ident_runtil(pair->item->hier, '.');

   fprintf(f, "<br><table class=\"cbt\">");

   cover_print_bin_header_legacy(f, pkind, 1, "State");

   int n_pairs = 0;
   do {
      ident_t state_name = ident_rfrom(pair->item->hier, '.');
      cover_print_bin_legacy(f, pair, COV_FLAG_STATE, pkind, 1, istr(state_name));
      n_pairs++;

      // End of the chain was hit
      if (pair == last)
         break;
      pair++;

      // A item with different prefix -> Not the same FSM.
      ident_t curr_prefix = ident_runtil(pair->item->hier, '.');
      if (!ident_starts_with(curr_prefix, first_prefix))
         break;

   } while (1);

   fprintf(f, "</table>");

   return n_pairs;
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
      step = 1;
      fprintf(f, "    <p>");

      switch (curr->item->kind) {
      case COV_ITEM_STMT:
         if (pkind == PAIR_UNCOVERED)
            cover_print_get_exclude_button(f, curr->item, 0, false);
         if (pkind == PAIR_EXCLUDED) {
            // No un-reachability on statements so far
            assert(curr->item->unrc_msk == 0);
            fprintf(f, "<div style=\"float: right\"><b>Excluded due to:</b> Exclude file</div>");
         }

         cover_print_code_loc(f, curr);
         break;

      case COV_ITEM_BRANCH:
         cover_print_code_loc(f, curr);
         cover_print_bins_legacy(f, curr, pkind);
         break;

      case COV_ITEM_TOGGLE:
         if (curr->item->flags & COV_FLAG_TOGGLE_SIGNAL)
            fprintf(f, "<h3>Signal:</h3>");
         else if (curr->item->flags & COV_FLAG_TOGGLE_PORT)
            fprintf(f, "<h3>Port:</h3>");

         const char *sig_name = istr(ident_runtil(curr->item->hier, '.'));
         sig_name += curr->item->sig_pos;
         fprintf(f, "&nbsp;<code>%s</code>", sig_name);

         cover_print_bin_header(f, pkind, 2, "From", "To");
         cover_print_bins(f, curr, pkind);
         step = curr->item->num;
         break;

      case COV_ITEM_EXPRESSION:
         cover_print_code_loc(f, curr);
         cover_print_bins_legacy(f, curr, pkind);
         break;

      case COV_ITEM_STATE:
         cover_print_code_loc(f, curr);
         step = cover_print_fsm_table(f, curr, pkind, last);
         break;

      case COV_ITEM_FUNCTIONAL:
         if (pkind == PAIR_UNCOVERED)
            cover_print_get_exclude_button(f, curr->item, 0, false);
         cover_print_code_loc(f, curr);
         break;

      default:
         fatal("unsupported type of code coverage: %d !", curr->item->kind);
      }

      fprintf(f, "<hr>");
      fprintf(f, "</p>\n");

      curr += step;

   } while (curr <= last);
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
      fprintf(f, "FSM state");
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
         fprintf(f, "sequences:");
      fprintf(f, "</h2>\n");

      fprintf(f, "  <section style=\"padding:0px 10px;\">\n");
      cover_print_pairs(f, first_pair, pkind, n);
      fprintf(f, "  </section>\n\n");
   }

   fprintf(f, "</div>\n");
}

static void cover_print_hierarchy_guts(FILE *f, cover_report_ctx_t *ctx)
{
   fprintf(f, "<div class=\"tab\">"
              "   <button class=\"tablinks\" onclick=\"selectCoverage(event, 'Statement')\" id=\"defaultOpen\">Statement</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Branch')\">Branch</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Toggle')\">Toggle</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Expression')\">Expression</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'FSM state')\">FSM state</button>\n"
              "   <button class=\"tablinks\" style=\"margin-left:10px;\" onclick=\"selectCoverage(event, 'Functional')\">Functional</button>\n"
              "</div>\n\n");

   cover_print_chain(f, ctx->data, &(ctx->ch_stmt), COV_ITEM_STMT);
   cover_print_chain(f, ctx->data, &(ctx->ch_branch), COV_ITEM_BRANCH);
   cover_print_chain(f, ctx->data, &(ctx->ch_toggle), COV_ITEM_TOGGLE);
   cover_print_chain(f, ctx->data, &(ctx->ch_expression), COV_ITEM_EXPRESSION);
   cover_print_chain(f, ctx->data, &(ctx->ch_state), COV_ITEM_STATE);
   cover_print_chain(f, ctx->data, &(ctx->ch_functional), COV_ITEM_FUNCTIONAL);

   fprintf(f, "<script>\n"
              "   document.getElementById(\"defaultOpen\").click();"
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
              "</script>\n");
}

// TODO: Remove once all coverage kinds keep run-time data as counters
static int cover_append_to_chain_legacy(cover_chain_t *chain, cover_item_t *item,
                                        cover_line_t *line, unsigned hits,
                                        unsigned misses, unsigned excludes,
                                        int limit)
{
   int rv = 0;
   if (hits) {
      if (chain->n_hits <= limit) {
         if (chain->n_hits == chain->alloc_hits) {
            chain->alloc_hits *= 2;
            chain->hits = xrealloc_array(chain->hits, chain->alloc_hits,
                                       sizeof(cover_pair_t));
         }
         chain->hits[chain->n_hits].item = item;
         chain->hits[chain->n_hits].line = line;
         chain->hits[chain->n_hits].flags = hits;
         chain->n_hits++;
      }
      else
         rv++;
   }

   if (misses) {
      if (chain->n_miss <= limit) {
         if (chain->n_miss == chain->alloc_miss) {
            chain->alloc_miss *= 2;
            chain->miss = xrealloc_array(chain->miss, chain->alloc_miss,
                                       sizeof(cover_pair_t));
         }
         chain->miss[chain->n_miss].item = item;
         chain->miss[chain->n_miss].line = line;
         chain->miss[chain->n_miss].flags = misses;
         chain->n_miss++;
      }
      else
         rv++;
   }

   if (excludes) {
      if (chain->n_excl <= limit) {
         if (chain->n_excl == chain->alloc_excl) {
            chain->alloc_excl *= 2;
            chain->excl = xrealloc_array(chain->excl, chain->alloc_excl,
                                       sizeof(cover_pair_t));
         }
         chain->excl[chain->n_excl].item = item;
         chain->excl[chain->n_excl].line = line;
         chain->excl[chain->n_excl].flags = excludes;
         chain->n_excl++;
      }
      else
         rv++;
   }

   return rv;
}

static bool cover_bin_unreachable(cover_report_ctx_t *ctx, cover_item_t *item)
{
   if ((ctx->data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Toggle items remember unreachability in run-time data. Must check item kind
   // not to get false unreachability on statement items. Excludes both bins
   // automatically!
   if (item->kind == COV_ITEM_TOGGLE && ((item->data & COV_FLAG_UNREACHABLE) != 0)) {
      item->flags |= COV_FLAG_UNREACHABLE;
      return true;
   }

   return false;
}

static bool cover_bin_unreachable_legacy(cover_report_ctx_t *ctx, cover_item_t *item,
                                  unsigned flag)
{
   if ((ctx->data->mask & COVER_MASK_EXCLUDE_UNREACHABLE) == 0)
      return false;

   // Expression items remembered unreachability via dedicated mask
   if (item->kind == COV_ITEM_EXPRESSION && ((flag & item->unrc_msk) != 0))
      return true;

   return false;
}

// TODO: Remove once all coverage kinds keep run-time data as counters!
static void cover_item_to_chain_legacy(cover_report_ctx_t *ctx, cover_item_t *item,
                                       cover_flags_t flag, unsigned *hits,
                                       unsigned *misses, unsigned *excludes)
{
   unsigned *flat_total;
   unsigned *nested_total;
   unsigned *flat_hits;
   unsigned *nested_hits;

   switch (item->kind) {
   case COV_ITEM_BRANCH:
      flat_total = &(ctx->flat_stats.total_branches);
      nested_total = &(ctx->nested_stats.total_branches);
      flat_hits = &(ctx->flat_stats.hit_branches);
      nested_hits = &(ctx->nested_stats.hit_branches);
      break;
   case COV_ITEM_TOGGLE:
      flat_total = &(ctx->flat_stats.total_toggles);
      nested_total = &(ctx->nested_stats.total_toggles);
      flat_hits = &(ctx->flat_stats.hit_toggles);
      nested_hits = &(ctx->nested_stats.hit_toggles);
      break;
   case COV_ITEM_EXPRESSION:
      flat_total = &(ctx->flat_stats.total_expressions);
      nested_total = &(ctx->nested_stats.total_expressions);
      flat_hits = &(ctx->flat_stats.hit_expressions);
      nested_hits = &(ctx->nested_stats.hit_expressions);
      break;
   case COV_ITEM_STATE:
      flat_total = &(ctx->flat_stats.total_states);
      nested_total = &(ctx->nested_stats.total_states);
      flat_hits = &(ctx->flat_stats.hit_states);
      nested_hits = &(ctx->nested_stats.hit_states);
      break;
   default:
      fatal("unsupported type of code coverage: %d !", item->kind);
   }

   (*flat_total)++;
   (*nested_total)++;

   if (item->data & flag) {
      (*flat_hits)++;
      (*nested_hits)++;
      (*hits) |= flag;
   }
   else if ((item->excl_msk & flag) || cover_bin_unreachable_legacy(ctx, item, flag)) {
      (*flat_hits)++;
      (*nested_hits)++;
      (*excludes) |= flag;
   }
   else
      (*misses) |= flag;
}

// TODO: Remove "flag" from "cover_pair_t" once all cover item kinds are reworked
//       to contain only single bin. It will not be needed then!
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
            first_chn_item->num = 1;                                             \
         }                                                                       \
         else                                                                    \
            first_chn_item->num++;                                               \
      } while (0);


static int cover_append_item_to_chain(cover_report_ctx_t *ctx, cover_item_t *first_item,
                                      cover_line_t *line, int limit, int *skipped)
{
   unsigned *flat_total;
   unsigned *nested_total;
   unsigned *flat_hits;
   unsigned *nested_hits;
   cover_chain_t *chn;

   switch (first_item->kind) {
   case COV_ITEM_TOGGLE:
      flat_total = &(ctx->flat_stats.total_toggles);
      nested_total = &(ctx->nested_stats.total_toggles);
      flat_hits = &(ctx->flat_stats.hit_toggles);
      nested_hits = &(ctx->nested_stats.hit_toggles);
      chn = &(ctx->ch_toggle);
      break;
   default:
      fatal("unsupported type of code coverage: %d !", first_item->kind);
   }

   // Process all consecutive cover_items in belonging to the same RTL construct
   // "first_item->num" gives the number of items to process since cover item emit.
   // Re-distribute so that this number is valid even if the items are sorted into
   // "hit", "miss" and "exclude" pair lists. The "num" in the original cover_item will
   // get corrupted, but "num" for each first item in each of the pair lists will
   // correspond to how many items within that given pair list correspond to the
   // same RTL construct. This re-sorting then simplifies coverage reporting.
   cover_item_t *first_hits_item = NULL;
   cover_item_t *first_miss_item = NULL;
   cover_item_t *first_excl_item = NULL;

   int n_steps = first_item->num;
   cover_item_t *last_item = first_item + n_steps - 1;

   for (cover_item_t *curr_item = first_item; curr_item <= last_item; curr_item++) {
      (*flat_total)++;
      (*nested_total)++;

      if (curr_item->data > 0) {
         (*flat_hits)++;
         (*nested_hits)++;

         if (chn->n_hits > limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, hits, first_hits_item, curr_item, line)
      }
      else if ((curr_item->flags & COV_FLAG_EXCLUDED) ||
                cover_bin_unreachable(ctx, curr_item)) {
         (*flat_hits)++;
         (*nested_hits)++;

         if (chn->n_excl > limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, excl, first_excl_item, curr_item, line)
      }
      else {
         if (chn->n_miss > limit) {
            (*skipped)++;
            curr_item++;
            continue;
         }

         CHAIN_APPEND(chn, miss, first_miss_item, curr_item, line)
      }
   };

   return n_steps;
}

static void cover_report_scope(cover_report_ctx_t *ctx,
                               cover_scope_t *s, const char *dir,
                               FILE *summf, int *skipped)
{
   for (int i = 0; i < s->items.count;) {
      int step = 1;
      cover_item_t *item = &(s->items.items[i]);
      assert(item->loc.file_ref == s->loc.file_ref);

      cover_file_t *f_src = cover_file_for_scope(s);
      if (f_src == NULL) {
         continue;
      }

      cover_line_t *line = &(f_src->lines[item->loc.first_line-1]);

      unsigned hits = 0;
      unsigned misses = 0;
      unsigned excludes = 0;
      int limit = ctx->data->report_item_limit;

      switch (item->kind) {
      case COV_ITEM_STMT:
         (ctx->flat_stats.total_stmts)++;
         (ctx->nested_stats.total_stmts)++;

         hits = (item->data != 0);
         misses = (item->data == 0) && (item->excl_msk == 0);
         excludes = (item->data == 0) && (item->excl_msk != 0);

         if (hits | excludes) {
            (ctx->flat_stats.hit_stmts)++;
            (ctx->nested_stats.hit_stmts)++;
         }
         *skipped += cover_append_to_chain_legacy(&(ctx->ch_stmt), item, line,
                                                  hits, misses, excludes, limit);
         break;

      case COV_ITEM_BRANCH:
         if (item->flags & COV_FLAG_CHOICE) {    // Case, with select
            ctx->flat_stats.total_branches++;
            ctx->nested_stats.total_branches++;

            if (item->data > 0) {
               ctx->flat_stats.hit_branches++;
               ctx->nested_stats.hit_branches++;
               hits |= COV_FLAG_CHOICE;
            }
            else if (item->excl_msk & COV_FLAG_TRUE) {
               ctx->flat_stats.hit_branches++;
               ctx->nested_stats.hit_branches++;
               hits |= COV_FLAG_CHOICE;
            }
            else
               misses |= COV_FLAG_CHOICE;
         }
         else if (item->num == 2) {  // If/else, when else
            ctx->flat_stats.total_branches += 2;
            ctx->nested_stats.total_branches += 2;

            if (item[0].data > 0 || (item[0].excl_msk & COV_FLAG_TRUE)) {
               ctx->flat_stats.hit_branches++;
               ctx->nested_stats.hit_branches++;
               hits |= COV_FLAG_TRUE;
            }
            else
               misses |= COV_FLAG_TRUE;

            if (item[1].data > 0 || (item[1].excl_msk & COV_FLAG_FALSE)) {
               ctx->flat_stats.hit_branches++;
               ctx->nested_stats.hit_branches++;
               hits |= COV_FLAG_FALSE;
            }
            else
               misses |= COV_FLAG_FALSE;
         }

         *skipped += cover_append_to_chain_legacy(&(ctx->ch_branch), item, line,
                                                  hits, misses, excludes, limit);
         break;

      case COV_ITEM_TOGGLE:
         step = cover_append_item_to_chain(ctx, item, line, limit, skipped);
         break;

      case COV_ITEM_EXPRESSION:
         if (item->flags & COV_FLAG_00)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_00,
                                       &hits, &misses, &excludes);
         if (item->flags & COV_FLAG_01)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_01,
                                       &hits, &misses, &excludes);
         if (item->flags & COV_FLAG_10)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_10,
                                       &hits, &misses, &excludes);
         if (item->flags & COV_FLAG_11)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_11,
                                       &hits, &misses, &excludes);
         if (item->flags & COV_FLAG_TRUE)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_TRUE,
                                       &hits, &misses, &excludes);
         if (item->flags & COV_FLAG_FALSE)
            cover_item_to_chain_legacy(ctx, item, COV_FLAG_FALSE,
                                       &hits, &misses, &excludes);

         *skipped += cover_append_to_chain_legacy(&(ctx->ch_expression), item, line,
                                                  hits, misses, excludes, limit);
         break;

      case COV_ITEM_STATE:
         cover_item_to_chain_legacy(ctx, item, COV_FLAG_STATE, &hits, &misses, &excludes);
         *skipped += cover_append_to_chain_legacy(&(ctx->ch_state), item, line,
                                                  hits, misses, excludes, limit);
         break;

      case COV_ITEM_FUNCTIONAL:
         (ctx->flat_stats.total_functional)++;
         (ctx->nested_stats.total_functional)++;

         hits = (item->data != 0);
         misses = (item->data == 0) && (item->excl_msk == 0);
         excludes = (item->data == 0) && (item->excl_msk != 0);

         if (hits | excludes) {
            (ctx->flat_stats.hit_functional)++;
            (ctx->nested_stats.hit_functional)++;
         }
         *skipped += cover_append_to_chain_legacy(&(ctx->ch_functional), item, line,
                                                  hits, misses, excludes, limit);
         break;

      default:
         fatal("unsupported type of code coverage: %d !", item->kind);
      }

      i += step;
   }

   cover_report_children(ctx, s, dir, summf, skipped);
}

static void cover_report_hierarchy(cover_report_ctx_t *ctx,
                                   cover_scope_t *s, const char *dir)
{
   char *hier LOCAL = xasprintf("%s/%s.html", dir, istr(s->hier));

   // TODO: Handle escaped identifiers in hierarchy path!
   FILE *f = fopen(hier, "w");
   if (f == NULL)
      fatal("failed to open report file: %s\n", hier);

   INIT_CHAIN(ctx, ch_stmt);
   INIT_CHAIN(ctx, ch_branch);
   INIT_CHAIN(ctx, ch_toggle);
   INIT_CHAIN(ctx, ch_expression);
   INIT_CHAIN(ctx, ch_state);
   INIT_CHAIN(ctx, ch_functional);

   cover_print_html_header(f);
   cover_print_navigation_tree(f, ctx, s);
   cover_print_file_and_inst(f, ctx, s);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Sub-instances:\n</h2>\n\n");
   cover_print_hierarchy_header(f);

   int skipped = 0;
   cover_report_children(ctx, s, dir, f, &skipped);

   cover_print_hierarchy_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Current Instance:\n</h2>\n\n");
   cover_print_hierarchy_header(f);
   cover_print_hierarchy_summary(f, ctx, s->hier, false, true, true);
   cover_print_hierarchy_footer(f);

   fprintf(f, "<h2 style=\"margin-left: " MARGIN_LEFT ";\">\n  Details:\n</h2>\n\n");
   if (skipped)
      fprintf(f, "<h3 style=\"margin-left: " MARGIN_LEFT ";\">The limit of "
                 "printed items was reached (%d). Total %d items are not "
                 "displayed.</h3>\n\n", ctx->data->report_item_limit, skipped);
   cover_print_hierarchy_guts(f, ctx);
   cover_print_timestamp(f);

   fclose(f);
}

static void cover_report_children(cover_report_ctx_t *ctx,
                                  cover_scope_t *s, const char *dir,
                                  FILE *summf, int *skipped)
{
   for (int i = 0; i < s->children.count; i++) {
      cover_scope_t *it = s->children.items[i];
      if (it->type == CSCOPE_INSTANCE) {
         // Collect coverage of sub-block
         cover_report_ctx_t sub_ctx = {};
         sub_ctx.parent = ctx;
         sub_ctx.data = ctx->data;
         sub_ctx.lvl = ctx->lvl + 2;

         cover_report_hierarchy(&sub_ctx, it, dir);

         cover_print_hierarchy_summary(summf, &sub_ctx,
                                       it->hier, false, false, false);

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
      }
      else
         cover_report_scope(ctx, it, dir, summf, skipped);
   }
}

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
   cover_print_hierarchy_header(f);

   for (int i = 0; i < data->root_scope->children.count; i++) {
      cover_scope_t *child = AGET(data->root_scope->children, i);
      cover_report_ctx_t top_ctx = {};

      top_ctx.data = data;
      data->report_item_limit = item_limit;

      cover_report_hierarchy(&top_ctx, child, subdir);
      cover_print_hierarchy_summary(f, &top_ctx, child->hier, true, true, false);
   }

   cover_print_hierarchy_footer(f);
   cover_print_timestamp(f);

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

   fclose(f);
}
