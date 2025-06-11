//
//  Copyright (C) 2022-2025 Nick Gasson
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

void vlog_preprocess(text_buf_t *tb, bool precise);
vlog_node_t vlog_parse(void);
void vlog_check(vlog_node_t v);
void vlog_dump(vlog_node_t v, int indent);
void vlog_simp(vlog_node_t mod);
void vlog_trans(vlog_node_t mod, tree_t out);
vcode_unit_t vlog_lower(unit_registry_t *ur, mir_context_t *mc,
                        vlog_node_t mod);
vlog_node_t vlog_new_instance(vlog_node_t mod, vlog_node_t inst,
                              ident_t prefix);

#endif  // _VLOG_PHASE_H
