//
//  Copyright (C) 2026  Nick Gasson
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

#ifndef _HIER_H
#define _HIER_H

#include "prim.h"
#include "vlog/vlog-node.h"

// The scope-naming model after the wrapper/template split:
//
//   * Templates live in the MIR map under their module short name
//     (e.g. WORK.MOD#0); they hold variable declarations.
//
//   * Per-instance wrappers live in the MIR map under
//     `<dotted>$instance`; they hold init code and are looked up by
//     create_scope (rt/model.c) which appends $instance to the
//     scope's dotted name.
//
//   * The bare dotted path (e.g. WORK.TOP.U_GLBL.glbl) is registered
//     as a MIR alias of the template by vlog_lower_block.  The
//     hier-ref resolver emits the dotted path on I_IDENT2;
//     instance_init uses the dotted path as its PUTPRIV key.  All
//     three sites name the scope identically so PUTPRIV/GETPRIV pair
//     on the same JIT handle and link_package always lands on the
//     template.
//
// Consequence: the elaborator no longer mints per-clone "alias"
// idents.  The dotted path IS the per-scope identity at every site.

// Language tag for a scope-tree node.  Used by the hier-ref resolver
// to decide whether a node owns lookup-able decls (Verilog) or is a
// transparent waypoint on the path (VHDL wrapper).
typedef enum {
   HIER_LANG_VHDL,
   HIER_LANG_VLOG,
} hier_lang_t;

// A node in the elaborated scope tree.  Every top-level architecture,
// Verilog module, generate iteration, named block, and VHDL sub-block
// gets one.  Nodes are keyed by `dotted` in the per-elaboration
// scope_tree hash on the synthetic root; children are enumerable
// because every node knows its parent, and callers that need "child
// X of parent Y" can probe `ident_prefix(Y->dotted, X, '.')`.
typedef struct _hier_node hier_node_t;
struct _hier_node {
   ident_t       dotted;      // unique full hierarchical path
   ident_t       label;       // short name under parent (tree_ident)
   hier_node_t  *parent;      // NULL for children of the synthetic root
   tree_t        tree_body;   // T_BLOCK of the elaborated scope
   vlog_node_t   vlog_body;   // V_INST_BODY / V_BLOCK, NULL for VHDL
   hier_lang_t   lang;
};

#endif  // _HIER_H
