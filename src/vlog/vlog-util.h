//
//  Copyright (C) 2024-2025 Nick Gasson
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

#ifndef _VLOG_UTIL_H
#define _VLOG_UTIL_H

#include "prim.h"

bool vlog_is_net(vlog_node_t v);
unsigned vlog_dimensions(vlog_node_t v);
bool vlog_get_const(vlog_node_t v, int64_t *value);
bool vlog_is_const(vlog_node_t v);
bool vlog_is_up(vlog_node_t v);
bool vlog_bounds(vlog_node_t v, int64_t *left, int64_t *right);
unsigned vlog_size(vlog_node_t v);
bool is_top_level(vlog_node_t v);
bool is_data_type(vlog_node_t v);
bool is_implicit_data_type(vlog_node_t v);
vlog_node_t vlog_longest_static_prefix(vlog_node_t v);
bool vlog_equal_node(vlog_node_t a, vlog_node_t b);
uint32_t vlog_hash_node(vlog_node_t v);
vlog_node_t vlog_get_type(vlog_node_t v);
vlog_node_t vlog_get_dim(vlog_node_t v, int n);

// True when the node carries per-instance state that the resolver
// or lowering writes onto the IR slot directly, and therefore must
// be deep-copied per clone / per generate iteration.  Single source
// of truth consulted by copy_instance_pred, copy_generate_pred,
// elab_module_needs_per_clone_state and the elab pre-stamp pass.
// Adding a new scope kind that needs per-clone state means
// extending only this function.
bool vlog_has_per_clone_state(vlog_node_t v);

// Canonical hierarchical-scope name encoder.  All consumers of the
// user-visible hierarchical name (lowering's link_package ident, VPI
// vpiFullName, VCD writer, %m runtime, reheat serialiser) must go
// through this helper rather than reading vlog_ident directly.
ident_t vlog_canonical_scope_name(vlog_node_t body);

#define CANNOT_HANDLE(v) do {                                           \
      fatal_at(vlog_loc(v), "cannot handle %s in %s" ,                  \
               vlog_kind_str(vlog_kind(v)), __FUNCTION__);              \
   } while (0)

#endif  // _VLOG_UTIL_H
