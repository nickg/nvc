//
//  Copyright (C) 2022 Nick Gasson
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

#ifndef _VLOG_PHASE_H
#define _VLOG_PHASE_H

#include "prim.h"

#include <stdio.h>

void vlog_preprocess(FILE *f, text_buf_t *tb);
vlog_node_t vlog_parse(void);
void vlog_check(vlog_node_t v);
void vlog_dump(vlog_node_t v, int indent);
void vlog_lower(unit_registry_t *ur, tree_t wrap, lower_unit_t *parent);

#endif  // _VLOG_PHASE_H
