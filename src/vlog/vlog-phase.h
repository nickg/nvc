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
void vlog_lower_deferred(mir_unit_t *mu, object_t *obj);
void vlog_lower_udp(mir_unit_t *mu, object_t *obj);
void vlog_lower_block(mir_context_t *mc, ident_t parent, tree_t b);
mir_unit_t *vlog_lower_thunk(mir_context_t *mc, ident_t parent, vlog_node_t v);
vlog_node_t vlog_new_instance(vlog_node_t mod, vlog_node_t inst,
                              ident_t prefix);
vlog_node_t vlog_generate_instance(vlog_node_t v, vlog_node_t genvar,
                                   int32_t value, ident_t prefix);

#endif  // _VLOG_PHASE_H
