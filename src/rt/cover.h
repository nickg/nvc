//
//  Copyright (C) 2013-2021  Nick Gasson
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

#ifndef _COVER_H
#define _COVER_H

#include "util.h"
#include "tree.h"

typedef struct _cover_tagging cover_tagging_t;

cover_tagging_t *cover_tag(tree_t top);
void cover_report(tree_t top, cover_tagging_t *tagging,
                  const int32_t *stmts, const int32_t *conds);
bool cover_is_tagged(cover_tagging_t *tagging, tree_t t,
                     int32_t *tag, int32_t *sub_cond);
void cover_count_tags(cover_tagging_t *tagging, int32_t *n_stmts,
                      int32_t *n_conds);
cover_tagging_t *cover_read_tags(tree_t top);

#endif  // _COVER_H
