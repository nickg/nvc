//
//  Copyright (C) 2021  Nick Gasson
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

#ifndef _ELAB_H
#define _ELAB_H

#include "prim.h"

#include <stdbool.h>

typedef enum {
   E_ROOT,
   E_SCOPE,
   E_SIGNAL,
   E_PROCESS,
   E_NEXUS,
   E_PORT,
   E_IMPLICIT,

   E_LAST_NODE_KIND
} e_kind_t;

typedef enum {
   E_F_LAST_VALUE = (1 << 0),
   E_F_CONTIGUOUS = (1 << 1),
   E_F_POSTPONED  = (1 << 2),
   E_F_RESOLVED   = (1 << 3),
   E_F_CONV_FUNC  = (1 << 4),
   E_F_REGISTER   = (1 << 5),
} e_flags_t;

#define NEXUS_POS_INVALID ~0u

e_node_t e_new(e_kind_t kind);
e_kind_t e_kind(e_node_t e);
const char *e_kind_str(e_kind_t t);
void e_dump(e_node_t e);
e_node_t e_split_nexus(e_node_t root, e_node_t orig, unsigned width);
void e_write(e_node_t e, fbuf_t *fbuf);
e_node_t e_read(fbuf_t *fbuf);
void e_clean_nexus_array(e_node_t root);
void e_collapse_port(e_node_t root, unsigned pos, e_node_t old, e_node_t port);

const loc_t *e_loc(e_node_t e);
void e_set_loc(e_node_t e, const loc_t *loc);

ident_t e_ident(e_node_t e);
void e_set_ident(e_node_t e, ident_t i);

unsigned e_pos(e_node_t e);
void e_set_pos(e_node_t e, unsigned pos);
bool e_has_pos(e_node_t e);

e_flags_t e_flags(e_node_t e);
void e_set_flag(e_node_t e, e_flags_t mask);

ident_t e_instance(e_node_t e);
void e_set_instance(e_node_t e, ident_t i);

ident_t e_path(e_node_t e);
void e_set_path(e_node_t e, ident_t i);

unsigned e_scopes(e_node_t e);
e_node_t e_scope(e_node_t e, unsigned n);
void e_add_scope(e_node_t e, e_node_t s);

unsigned e_procs(e_node_t e);
e_node_t e_proc(e_node_t e, unsigned n);
void e_add_proc(e_node_t e, e_node_t p);

unsigned e_sources(e_node_t e);
e_node_t e_source(e_node_t e, unsigned n);
void e_add_source(e_node_t e, e_node_t s);

unsigned e_outputs(e_node_t e);
e_node_t e_output(e_node_t e, unsigned n);
void e_add_output(e_node_t e, e_node_t s);

unsigned e_deps(e_node_t e);
ident_t e_dep(e_node_t e, unsigned n);
void e_add_dep(e_node_t e, ident_t i);

e_node_t e_parent(e_node_t e);
void e_set_parent(e_node_t e, e_node_t p);
bool e_has_parent(e_node_t e);

unsigned e_signals(e_node_t e);
e_node_t e_signal(e_node_t e, unsigned n);
void e_add_signal(e_node_t e, e_node_t s);

unsigned e_triggers(e_node_t e);
e_node_t e_trigger(e_node_t e, unsigned n);
void e_add_trigger(e_node_t e, e_node_t t);

unsigned e_nexuses(e_node_t e);
e_node_t e_nexus(e_node_t e, unsigned n);
void e_add_nexus(e_node_t e, e_node_t n);
void e_change_nexus(e_node_t e, unsigned o, e_node_t n);
void e_insert_nexus(e_node_t e, e_node_t after, e_node_t new);

unsigned e_width(e_node_t e);
void e_set_width(e_node_t e, unsigned w);

unsigned e_size(e_node_t e);
void e_set_size(e_node_t e, unsigned s);

bool e_has_vcode(e_node_t e);
ident_t e_vcode(e_node_t e);
void e_set_vcode(e_node_t e, ident_t vunit);

type_t e_type(e_node_t e);
void e_set_type(e_node_t e, type_t type);

#endif  // _ELAB_H
