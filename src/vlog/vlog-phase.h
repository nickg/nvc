//
//  Copyright (C) 2022-2026 Nick Gasson
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
void vlog_lower_udp(mir_unit_t *mu, object_t *obj);
void vlog_lower_block(mir_context_t *mc, ident_t parent, ident_t self_alias,
                      tree_t b);

// The canonical per-scope alias rule: inst_alias ?: cloned ?: dotted.
// Each module (elab, reheat) provides its own static inline
// vlog_scope_alias() applying this rule to its local ctx type — the
// rule is short enough that replicating the one-line body is cheaper
// than exporting the ctx types across modules.  Every site that
// must agree on a scope's MIR name (vlog_lower_block, the elab-time
// hier-ref resolver, reheat_block) routes through its module's
// vlog_scope_alias so link_package never sees a drifted ident.

void vlog_lower_instance(mir_context_t *mc, vlog_node_t body, ident_t parent,
                         tree_t trans);
mir_unit_t *vlog_lower_thunk(mir_context_t *mc, ident_t parent, vlog_node_t v);
vlog_node_t vlog_new_instance(vlog_node_t mod, vlog_node_t inst, ident_t id);
vlog_node_t vlog_generate_instance(vlog_node_t v, vlog_node_t genvar,
                                   int32_t value, ident_t prefix);

#endif  // _VLOG_PHASE_H
