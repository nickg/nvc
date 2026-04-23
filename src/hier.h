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

// A scope's identity in the elaborated instance hierarchy.  Embedded
// in every context type that represents a live scope (elab_ctx_t,
// reheat_ctx_t, vlog_deferred_body_t …).  Three fields because a
// scope can be named three ways:
//
//   inst_alias — per-clone alias built from ancestor path (NULL when
//                the scope needs no cloning).
//   cloned    — the shared module identity (e.g. WORK.MOD#0): same
//                for every clone of a given module.
//   dotted    — the unique dotted path (e.g. WORK.TOP.u.v).
//
// Every site that must agree on the MIR alias for link_package
// consults hier_scope_alias() on this struct — that is the single
// rule that keeps registration (vlog_lower_block), the elab-time
// hier-ref resolver, and reheat pointing at the same ident.
typedef struct {
   ident_t inst_alias;
   ident_t cloned;
   ident_t dotted;
} hier_scope_t;

// Canonical per-scope alias: the per-clone inst_alias when one was
// minted, else the shared module name, else the dotted path.  If
// this ever diverges from what vlog_lower_block registers via
// mir_alias_unit, link_package silently misses at runtime.
static inline ident_t hier_scope_alias(const hier_scope_t *s)
{
   return s->inst_alias ?: s->cloned ?: s->dotted;
}

#endif  // _HIER_H
