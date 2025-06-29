//
//  Copyright (C) 2025  Nick Gasson
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

#ifndef _VLOG_SYMTAB_H
#define _VLOG_SYMTAB_H

#include "prim.h"
#include "vlog/vlog-node.h"

vlog_symtab_t *vlog_symtab_new(void);
void vlog_symtab_free(vlog_symtab_t *st);

void vlog_symtab_push(vlog_symtab_t *st, vlog_node_t v);
void vlog_symtab_pop(vlog_symtab_t *st);
void vlog_symtab_set_implicit(vlog_symtab_t *st, vlog_net_kind_t kind);

void vlog_symtab_lookup(vlog_symtab_t *st, vlog_node_t v);
void vlog_symtab_put(vlog_symtab_t *st, vlog_node_t v);
void vlog_symtab_poison(vlog_symtab_t *st, ident_t name);

vlog_node_t vlog_symtab_query(vlog_symtab_t *st, ident_t name);

#endif  // _VLOG_SYMTAB
