//
//  Copyright (C) 2013  Nick Gasson
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

#include "cover.h"

static ident_t stmt_tag_i;

static void cover_tag_stmts_fn(tree_t t, void *context)
{
   int *next = context;

   switch (tree_kind(t)) {
   case T_SIGNAL_ASSIGN:
   case T_ASSERT:
   case T_VAR_ASSIGN:
      tree_add_attr_int(t, stmt_tag_i, (*next)++);
      break;

   default:
      break;
   }
}

void cover_tag(tree_t top)
{
   stmt_tag_i = ident_new("stmt_tag");

   int line_tags = 0;
   tree_visit(top, cover_tag_stmts_fn, &line_tags);

   tree_add_attr_int(top, ident_new("stmt_tags"), line_tags);
}

void cover_report(tree_t top, const int32_t *stmts)
{
   const int ntags = tree_attr_int(top, ident_new("stmt_tags"), 0);
   for (int i = 0; i < ntags; i++) {
      printf("tag %d -> %d\n", i, stmts[i]);
   }
}
